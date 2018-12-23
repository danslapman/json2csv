{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Json2Csv (computePaths, navigate, jsonPathText) where

import Control.Lens ((^?))
import Control.Monad ((>=>))
import Data.Aeson
import Data.Aeson.Internal
import Data.Aeson.Lens
import Data.List (union)
import Data.List.Index
import qualified Data.HashMap.Strict as HM
import Data.Semigroup
import Data.Text (Text, intercalate)
import Data.Vector (toList)
import TextShow

concatUnion :: Eq a => [[a]] -> [a]
concatUnion = foldl1 union

prepend :: JSONPathElement -> [JSONPath] -> [JSONPath]
prepend prefix [] = [prefix : []]
prepend prefix path = fmap (\p -> prefix : p) path

computePaths :: Bool -> Value -> [JSONPath]
computePaths _ Null = []
computePaths _ (Bool _) = []
computePaths _ (Number _) = []
computePaths _ (String _) = []
computePaths False (Array arr) =
  concat . 
  (imap (\idx -> prepend (Index idx))) . 
  (fmap (computePaths False)) . 
  toList $ arr
computePaths True (Array arr) = concatUnion . (fmap (computePaths False)) . toList $ arr
computePaths _ (Object obj) =
  concatMap (uncurry (\key -> (prepend (Key key)))) . 
  HM.toList . 
  (HM.map (computePaths False)) $ obj

navigate :: JSONPath -> Value -> Maybe Value
navigate path =
  let stepFwd = \case
                  Key k -> (^? key k) :: Value -> Maybe Value
                  Index i -> (^? nth i) :: Value -> Maybe Value
      (firstStep : otherSteps) = (fmap stepFwd path)
  in foldl (>=>) firstStep otherSteps

jsonPathText :: JSONPath -> Text
jsonPathText path =
  let repr = \case
               Key k -> k
               Index i -> showt i
  in intercalate "." $ fmap repr path