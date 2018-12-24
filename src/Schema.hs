{-# LANGUAGE LambdaCase #-}

module Schema where

import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Typeable (Typeable)

data JSONPathElement = 
  Key Text
  | Iterator
  deriving (Eq, Show, Typeable, Ord)

type JSONPath = [JSONPathElement]

data JSONSchemaTree =
  Node JSONPathElement [JSONSchemaTree]
  | Leaf

type JsonSchema = [JSONSchemaTree]

hasSameRoot :: JSONPath -> JSONSchemaTree -> Bool
hasSameRoot path tree = case (tree, path) of
  ((Node el _), (h:_)) | el == h -> True
  otherwise -> False

toSchemaTree :: JSONPath -> JSONSchemaTree
toSchemaTree = 
  \case
    [] -> Leaf
    root:path -> Node root [toSchemaTree path]

(#+) :: JsonSchema -> JSONPath -> JsonSchema
(#+) schema path =
  let append path tree = case (tree, path) of
        (t, []) -> t
        ((Node el []), h:tail) | el == h ->
          Node el [toSchemaTree tail]
        ((Node el branches), h:tail) | el == h -> 
          Node el $ fmap (append tail) branches
        ((Node el branches), path) ->
          Node el ((toSchemaTree path):branches)
  in
    if any (hasSameRoot path) schema
    then fmap (\case 
      t | hasSameRoot path t -> append path t 
      t -> t) schema
    else (toSchemaTree path) : schema