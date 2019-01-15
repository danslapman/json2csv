{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Schema where

import Control.Applicative (pure)
import Control.Lens ((^?), (^..))
import Control.Monad (join)
import Data.Aeson
import Data.Aeson.Lens
import Data.Foldable (any, foldl', toList)
import Data.Maybe hiding (mapMaybe)
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, intercalate)
import Data.Typeable (Typeable)
import Deque as DQ
import DequePatterns
import DequeUtils
import GHC.Generics (Generic)
import Prelude hiding (any, foldl, foldl', head)

data JsonPathElement = 
  Key Text
  | Iterator
  deriving (Eq, Show, Typeable, Ord, Generic)

instance Hashable JsonPathElement

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
toSchema = foldl' (#+) empty

data JsonValueTree =
  ValueRoot JsonPathElement (Deque JsonValueTree)
  | SingleValue JsonPathElement Value
  | ValueArray (Deque Value)
  | TreeArray (Deque (Deque JsonValueTree))
  deriving (Eq, Show, Typeable)

type JsonTree = Deque JsonValueTree

extract :: JsonSchema -> Value -> JsonTree
extract schema value =
  let extractTree v schemaTree = 
        case schemaTree of
          PathEnd -> Nothing
          (PathNode el (PathEnd :|| D_)) ->
            case el of
              Key k -> SingleValue el <$> v ^? key k
              Iterator -> ValueArray <$> (maybeNeq $ fromList $ v ^.. values)
          (PathNode (el @ (Key k)) children) ->
            let keyValue = (v ^? key k)
                childrenExtractors = flip extractTree <$> children
                valueTrees = (\val -> (mapMaybe id) $ ($val) <$> childrenExtractors) <$> keyValue
            in ValueRoot el <$> valueTrees
          (PathNode Iterator children) ->
            let nodeValues = fromList $ v ^.. values
                childrenExtractors = flip extractTree <$> children
                nodeTrees = (\val -> (mapMaybe id) $ ($val) <$> childrenExtractors) <$> nodeValues
            in TreeArray <$> maybeNeq nodeTrees
  in (mapMaybe id) $ ((extractTree value) <$> schema)

genMaps :: JsonPath -> JsonValueTree -> Deque (HashMap Text Value)
genMaps jp jvt =
  case jvt of
    ValueRoot jpe trees -> xfold $ genMaps (jpe `snoc` jp) <$> trees
    SingleValue jpe value -> pure $ HM.singleton (jsonPathText $ jpe `snoc` jp) value
    ValueArray values -> HM.singleton (jsonPathText (Iterator `snoc` jp)) <$> values
    TreeArray trees -> join $ ((xfold . (genMaps (Iterator `snoc` jp) <$>)) <$> trees)

generateTuples :: JsonTree -> Deque (HashMap Text Value)
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

xfold :: Deque (Deque (HashMap Text Value)) -> Deque (HashMap Text Value)
xfold = foldl' (xseq HM.union) empty