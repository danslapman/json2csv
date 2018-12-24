{-# LANGUAGE LambdaCase #-}

module Schema where

import Data.Aeson
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Typeable (Typeable)

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
  in
    if any (hasSameRoot path) schema
    then fmap (\case 
      t | hasSameRoot path t -> append path t 
      t -> t) schema
    else (toSchemaTree path) : schema

toSchema :: [JsonPath] -> JsonSchema
toSchema = foldl (#+) []

data JsonValueTree =
  ValueRoot [JsonValueTree]
  | ValueNode Value

extract :: JsonSchemaTree -> Value -> Maybe JsonValueTree
extract schema value = undefined