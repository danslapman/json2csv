{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Schema where

import Control.Applicative (pure)
import Control.Monad (join)
import Data.Foldable (any, foldl, toList)
import Data.Maybe hiding (mapMaybe)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, intercalate)
import Data.Typeable (Typeable)
import Deque as DQ
import DequePatterns
import DequeUtils
import Prelude hiding (any, foldl, head)
import Text.JSON

data JsonPathElement = 
  Key Text
  | Iterator
  deriving (Eq, Show, Typeable, Ord)

type JsonPath = Deque JsonPathElement

data JsonSchemaTree =
  PathNode JsonPathElement (Deque JsonSchemaTree)
  | PathEnd
  deriving (Eq, Show, Typeable)

type JsonSchema = Deque JsonSchemaTree

hasSameRoot :: JsonPath -> JsonSchemaTree -> Bool
hasSameRoot path tree = case (tree, path) of
  ((PathNode el _), (h:||_)) | el == h -> True
  otherwise -> False

toSchemaTree :: JsonPath -> JsonSchemaTree
toSchemaTree = 
  \case
    D_ -> PathEnd
    root:||path -> PathNode root $ pure $ toSchemaTree path

(#+) :: JsonSchema -> JsonPath -> JsonSchema
(#+) schema path =
  let append path tree = case (tree, path) of
        (t, D_) -> t
        ((PathNode el D_), h:||tail) | el == h ->
          PathNode el $ pure $ toSchemaTree tail
        ((PathNode el branches), h:||tail) | el == h -> 
          PathNode el $ uniq $ branches #+ tail
        (PathEnd, path) -> toSchemaTree path
        (t, _) -> t
  in
    if any (hasSameRoot path) schema
    then (append path) <$> schema
    else toSchemaTree path `snoc` schema

toSchema :: Deque JsonPath -> JsonSchema
toSchema = foldl (#+) empty

data JsonValueTree =
  ValueRoot JsonPathElement (Deque JsonValueTree)
  | SingleValue JsonPathElement JSValue
  | ValueArray (Deque JSValue)
  | TreeArray (Deque (Deque JsonValueTree))
  deriving (Eq, Show, Typeable)

type JsonTree = Deque JsonValueTree

extractKey :: Text -> JSValue -> Maybe JSValue
extractKey key jsv =
  case jsv of
    JSObject jso ->
      case valFromObj (show key) jso of
        Ok j -> Just j
        Error _ -> Nothing
    otherwise -> Nothing

extractValues :: JSValue -> [JSValue]
extractValues jsv =
  case jsv of
    JSArray values -> values
    otherwise -> []

extract :: JsonSchema -> JSValue -> JsonTree
extract schema value =
  let extractTree v schemaTree = 
        case schemaTree of
          PathEnd -> Nothing
          (PathNode el (PathEnd :|| D_)) ->
            case el of
              Key k -> SingleValue el <$> extractKey k v
              Iterator -> ValueArray <$> (maybeNeq $ fromList $ extractValues v)
          (PathNode (el @ (Key k)) children) ->
            let keyValue = extractKey k v
                childrenExtractors = flip extractTree <$> children
                valueTrees = (\val -> (mapMaybe id) $ ($val) <$> childrenExtractors) <$> keyValue
            in ValueRoot el <$> valueTrees
          (PathNode Iterator children) ->
            let nodeValues = fromList $ extractValues v
                childrenExtractors = flip extractTree <$> children
                nodeTrees = (\val -> (mapMaybe id) $ ($val) <$> childrenExtractors) <$> nodeValues
            in TreeArray <$> maybeNeq nodeTrees
  in (mapMaybe id) $ ((extractTree value) <$> schema)

genMaps :: JsonPath -> JsonValueTree -> Deque (HashMap Text JSValue)
genMaps jp jvt =
  case jvt of
    ValueRoot jpe trees -> xfold $ genMaps (jpe `snoc` jp) <$> trees
    SingleValue jpe value -> pure $ HM.singleton (jsonPathText $ jpe `snoc` jp) value
    ValueArray values -> HM.singleton (jsonPathText (Iterator `snoc` jp)) <$> values
    TreeArray trees -> join $ ((xfold . (genMaps (Iterator `snoc` jp) <$>)) <$> trees)

generateTuples :: JsonTree -> Deque (HashMap Text JSValue)
generateTuples jTree = xfold $ (genMaps empty) <$> jTree

jsonPathText :: JsonPath -> Text
jsonPathText path =
  let repr = \case
                Key k -> k
                Iterator -> "$"
  in intercalate "." $ toList $ fmap repr path

xseq :: (a -> a -> a) -> Deque a -> Deque a -> Deque a
xseq _ va D_ = va
xseq _ D_ vb = vb
xseq f va vb = do
  a <- va
  b <- vb
  return $ f a b

xfold :: Deque (Deque (HashMap Text JSValue)) -> Deque (HashMap Text JSValue)
xfold = foldl (xseq HM.union) empty