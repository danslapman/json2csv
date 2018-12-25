{-# LANGUAGE LambdaCase #-}

module Schema where

import Control.Lens ((^?), (^..))
import Data.Aeson
import Data.Aeson.Lens
import Data.Maybe hiding (mapMaybe)
import Data.Text (Text)
import Data.Traversable
import Data.Typeable (Typeable)
import Data.Vector
import Prelude hiding (any, foldl)
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
          PathNode el $ branches #+ tail
        (PathEnd, path) -> toSchemaTree path
        (t, _) -> t
  in
    if any (hasSameRoot path) schema
    then fmap (append path) schema
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
