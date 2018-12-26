{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Schema where

import Control.Lens ((^?), (^..))
import Data.Aeson
import Data.Aeson.Lens
import Data.Maybe hiding (mapMaybe)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, intercalate)
import Data.Traversable
import Data.Typeable (Typeable)
import Data.Vector as V
import Prelude hiding (any, foldl, head)
import Util
import VecPats

data JsonPathElement = 
  Key Text
  | Iterator
  deriving (Eq, Show, Typeable, Ord)

type JsonPath = Vector JsonPathElement

data JsonSchemaTree =
  PathNode JsonPathElement (Vector JsonSchemaTree)
  | PathEnd
  deriving (Eq, Show, Typeable, Ord)

type JsonSchema = Vector JsonSchemaTree

hasSameRoot :: JsonPath -> JsonSchemaTree -> Bool
hasSameRoot path tree = case (tree, path) of
  ((PathNode el _), (h:::_)) | el == h -> True
  otherwise -> False

toSchemaTree :: JsonPath -> JsonSchemaTree
toSchemaTree = 
  \case
    V_ -> PathEnd
    root:::path -> PathNode root $ singleton $ toSchemaTree path

(#+) :: JsonSchema -> JsonPath -> JsonSchema
(#+) schema path =
  let append path tree = case (tree, path) of
        (t, V_) -> t
        ((PathNode el V_), h:::tail) | el == h ->
          PathNode el $ singleton $ toSchemaTree tail
        ((PathNode el branches), h:::tail) | el == h -> 
          PathNode el $ uniq $ branches #+ tail
        (PathEnd, path) -> toSchemaTree path
        (t, _) -> t
  in
    if any (hasSameRoot path) schema
    then (append path) <$> schema
    else schema `snoc` toSchemaTree path

toSchema :: Vector JsonPath -> JsonSchema
toSchema = foldl (#+) empty

data JsonValueTree =
  ValueRoot JsonPathElement (Vector JsonValueTree)
  | SingleValue JsonPathElement Value
  | ValueArray (Vector Value)
  | TreeArray (Vector (Vector JsonValueTree))
  deriving (Eq, Show, Typeable)

type JsonTree = Vector JsonValueTree

extract :: JsonSchema -> Value -> JsonTree
extract schema value =
  let extractTree v schemaTree = 
        case schemaTree of
          PathEnd -> Nothing
          (PathNode el (PathEnd ::: V_)) ->
            case el of
              Key k -> SingleValue el <$> v ^? key k
              Iterator -> ValueArray <$> (maybeNev $ fromList $ v ^.. values)
          (PathNode (el @ (Key k)) children) ->
            let keyValue = (v ^? key k)
                childrenExtractors = flip extractTree <$> children
                valueTrees = (\val -> (mapMaybe id) $ ($val) <$> childrenExtractors) <$> keyValue
            in ValueRoot el <$> valueTrees
          (PathNode Iterator children) ->
            let nodeValues = fromList $ v ^.. values
                childrenExtractors = flip extractTree <$> children
                nodeTrees = (\val -> (mapMaybe id) $ ($val) <$> childrenExtractors) <$> nodeValues
            in TreeArray <$> maybeNev nodeTrees
  in (mapMaybe id) $ ((extractTree value) <$> schema)

genMaps :: JsonPath -> JsonValueTree -> Vector (HashMap Text Value)
genMaps jp jvt =
  case jvt of
    ValueRoot jpe trees -> vconcat $ genMaps (jp `snoc` jpe) <$> trees
    SingleValue jpe value -> singleton $ HM.singleton (jsonPathText $ jp `snoc` jpe) value
    ValueArray values -> HM.singleton (jsonPathText jp) <$> values
    TreeArray trees -> vconcat $ ((xfold . (genMaps (jp `snoc` Iterator) <$>)) <$> trees)

generateTuples :: JsonTree -> Vector (HashMap Text Value)
generateTuples jTree = xfold $ (genMaps empty) <$> jTree

jsonPathText :: JsonPath -> Text
jsonPathText path =
  let repr = \case
                Key k -> k
                Iterator -> "$"
  in intercalate "." $ toList $ fmap repr path

xseq :: (a -> a -> a) -> Vector a -> Vector a -> Vector a
xseq _ va V_ = va
xseq _ V_ vb = vb
xseq f va vb = do
  a <- va
  b <- vb
  return $ f a b

xfold :: Vector (Vector (HashMap Text Value)) -> Vector (HashMap Text Value)
xfold = foldl (xseq HM.union) empty