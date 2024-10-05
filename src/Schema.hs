{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Schema where

import Control.DeepSeq
import Control.Lens ((^..), (^?))
import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Aeson.Lens
import Data.Foldable (any, foldl', toList)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable
import Data.Maybe hiding (mapMaybe)
import Data.Sequence
import Data.Text (Text, intercalate)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import SequenceUtils
import Prelude hiding (any, foldl, foldl', head)

data JsonPathElement
  = Key Text
  | Iterator
  deriving (Eq, Show, Typeable, Ord, Generic)

instance Hashable JsonPathElement

instance NFData JsonPathElement

type JsonPath = Seq JsonPathElement

data JsonSchemaTree
  = PathNode JsonPathElement (Seq JsonSchemaTree)
  | PathEnd
  deriving (Eq, Show, Typeable)

type JsonSchema = Seq JsonSchemaTree

hasSameRoot :: JsonPath -> JsonSchemaTree -> Bool
hasSameRoot path tree = case (tree, path) of
  (PathNode el _, h :<| _) | el == h -> True
  otherwise -> False

toSchemaTree :: JsonPath -> JsonSchemaTree
toSchemaTree =
  \case
    Empty -> PathEnd
    root :<| path -> PathNode root $ pure $ toSchemaTree path

(#+) :: JsonSchema -> JsonPath -> JsonSchema
(#+) schema path =
  let append path tree = case (tree, path) of
        (t, Empty) -> t
        (PathNode el Empty, h :<| tail)
          | el == h ->
              PathNode el $ pure $ toSchemaTree tail
        (PathNode el branches, h :<| tail)
          | el == h ->
              PathNode el $ uniq $ branches #+ tail
        (PathEnd, path) -> toSchemaTree path
        (t, _) -> t
   in if any (hasSameRoot path) schema
        then append path <$> schema
        else schema |> toSchemaTree path

toSchema :: Seq JsonPath -> JsonSchema
toSchema = foldl' (#+) empty

data JsonValueTree
  = ValueRoot JsonPathElement (Seq JsonValueTree)
  | SingleValue JsonPathElement Value
  | ValueArray (Seq Value)
  | TreeArray (Seq (Seq JsonValueTree))
  deriving (Eq, Show, Typeable)

type JsonTree = Seq JsonValueTree

extract :: JsonSchema -> Value -> JsonTree
extract schema value =
  let extractTree v schemaTree =
        case schemaTree of
          PathEnd -> Nothing
          (PathNode el (PathEnd :<| Empty)) ->
            case el of
              Key k -> SingleValue el <$> v ^? (key $ fromText k)
              Iterator -> ValueArray <$> (maybeNes $ fromList $ v ^.. values)
          (PathNode el@(Key k) children) ->
            let keyValue = (v ^? (key $ fromText k))
                childrenExtractors = flip extractTree <$> children
                valueTrees = (\val -> mapMaybe id $ ($ val) <$> childrenExtractors) <$> keyValue
             in ValueRoot el <$> valueTrees
          (PathNode Iterator children) ->
            let nodeValues = fromList $ v ^.. values
                childrenExtractors = flip extractTree <$> children
                nodeTrees = (\val -> mapMaybe id $ ($ val) <$> childrenExtractors) <$> nodeValues
             in TreeArray <$> maybeNes nodeTrees
   in mapMaybe id $ (extractTree value <$> schema)

genMaps :: Bool -> JsonPath -> JsonValueTree -> Seq (HashMap Text Value)
genMaps flat jp jvt =
  case (flat, jvt) of
    (_, ValueRoot jpe trees) -> xfold $ genMaps flat (jp |> jpe) <$> trees
    (_, SingleValue jpe value) -> pure $ HM.singleton (jsonPathText $ jp |> jpe) value
    (False, ValueArray values) -> HM.singleton (jsonPathText (jp |> Iterator)) <$> values
    (True, ValueArray values) -> HM.singleton (jsonPathText jp) <$> values
    (False, TreeArray trees) -> trees >>= (xfold . (genMaps flat (jp |> Iterator) <$>))
    (True, TreeArray trees) -> trees >>= (xfold . (genMaps flat jp <$>))

generateTuples :: Bool -> JsonTree -> Seq (HashMap Text Value)
generateTuples flat jTree = xfold $ genMaps flat empty <$> jTree

jsonPathText :: JsonPath -> Text
jsonPathText path =
  intercalate "." $ toList $ repr <$> path
  where
    repr = \case
      Key k -> k
      Iterator -> "$"

dropIterators :: JsonPath -> JsonPath
dropIterators jpath =
  mapMaybe takeKeys jpath
  where
    takeKeys = \case
      Key k -> Just $ Key k
      Iterator -> Nothing

xseq :: (a -> a -> a) -> Seq a -> Seq a -> Seq a
xseq _ va Empty = va
xseq _ Empty vb = vb
xseq f va vb = do
  a <- va
  b <- vb
  return $ f a b

xfold :: Seq (Seq (HashMap Text Value)) -> Seq (HashMap Text Value)
xfold = foldl' (xseq HM.union) empty