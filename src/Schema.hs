{-# LANGUAGE LambdaCase #-}

module Schema where

import Control.Lens ((^?), (^..))
import Data.Aeson
import Data.Aeson.Lens
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Traversable
import Data.Typeable (Typeable)
import Util

data JsonPathElement = 
  Key Text
  | Iterator
  deriving (Eq, Show, Typeable, Ord)

type JsonPath = [JsonPathElement]

data JsonSchemaTree =
  PathNode JsonPathElement [JsonSchemaTree]
  | PathEnd
  deriving (Eq, Show, Typeable, Ord)

type JsonSchema = [JsonSchemaTree]

hasSameRoot :: JsonPath -> JsonSchemaTree -> Bool
hasSameRoot path tree = case (tree, path) of
  ((PathNode el _), (h:_)) | el == h -> True
  otherwise -> False

toSchemaTree :: JsonPath -> JsonSchemaTree
toSchemaTree = 
  \case
    [] -> PathEnd
    root:path -> PathNode root [toSchemaTree path]

(#+) :: JsonSchema -> JsonPath -> JsonSchema
(#+) schema path =
  let append path tree = case (tree, path) of
        (t, []) -> t
        ((PathNode el []), h:tail) | el == h ->
          PathNode el [toSchemaTree tail]
        ((PathNode el branches), h:tail) | el == h -> 
          PathNode el $ branches #+ tail
        (PathEnd, path) -> toSchemaTree path
        (t, _) -> t
  in
    if any (hasSameRoot path) schema
    then fmap (append path) schema
    else (toSchemaTree path) : schema

toSchema :: [JsonPath] -> JsonSchema
toSchema = foldl (#+) []

data JsonValueTree =
  ValueRoot JsonPathElement [JsonValueTree]
  | SingleValue JsonPathElement Value
  | ValueArray [Value]
  | TreeArray [[JsonValueTree]]
  deriving (Eq, Show, Typeable)

type JsonTree = [JsonValueTree]

extract :: JsonSchema -> Value -> JsonTree
extract schema value =
  let extractTree v schemaTree = 
        case schemaTree of
          PathEnd -> Nothing
          (PathNode el [PathEnd]) ->
            case el of
              Key k -> SingleValue el <$> v ^? key k
              Iterator -> ValueArray <$> (maybeNel $ v ^.. values)
          (PathNode (el @ (Key k)) children) ->
            let keyValue = (v ^? key k)
                childrenExtractors = flip extractTree <$> children
                valueTrees = (\val -> catMaybes $ ($val) <$> childrenExtractors) <$> keyValue
            in ValueRoot el <$> valueTrees
          (PathNode Iterator children) ->
            let nodeValues = v ^.. values
                childrenExtractors = flip extractTree <$> children
                nodeTrees = (\val -> catMaybes $ ($val) <$> childrenExtractors) <$> nodeValues
            in TreeArray <$> maybeNel nodeTrees
  in catMaybes $ ((extractTree value) <$> schema)
