{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Json2Csv (computePaths, navigate, jsonPathText, showj) where

import Control.Lens ((^?))
import Control.Monad ((>=>))
import Data.Aeson
import Data.Aeson.Internal
import Data.Aeson.Lens
import Data.Maybe (catMaybes)
import Data.Foldable (find)
import Data.List (union, null)
import Data.List.Index
import qualified Data.HashMap.Strict as HM
import Data.Semigroup
import Data.Text (Text, intercalate, pack)
import Data.Vector (toList)
import qualified Data.Vector as V
import TextShow

concatUnion :: Eq a => [[a]] -> [a]
concatUnion = foldl1 union

prepend :: JSONPathElement -> [JSONPath] -> [JSONPath]
prepend prefix [] = [prefix : []]
prepend prefix path = fmap (\p -> prefix : p) path

nonEmptyJ :: Value -> Bool
nonEmptyJ Null = False
nonEmptyJ (Object o) | HM.null o = False
nonEmptyJ (Array a) | V.null a = False
nonEmptyJ _ = True

computePaths :: Bool -> Value -> Maybe [JSONPath]
computePaths _ Null = Just []
computePaths _ (Bool _) = Just []
computePaths _ (Number _) = Just []
computePaths _ (String _) = Just []
computePaths False (Array arr) =
  (find (not . null) . Just) .
  concat . 
  (imap (\idx -> prepend (Index idx))) .
  catMaybes . 
  (fmap (computePaths False)) . 
  toList $ arr
computePaths True (Array arr) = 
  (find (not . null) . Just) . 
  concatUnion . 
  catMaybes . 
  (fmap (computePaths False)) . 
  toList $ arr
computePaths _ (Object obj) =
  (find (not . null) . Just) .
  concatMap (uncurry (\key -> (prepend (Key key)))) . 
  HM.toList .
  HM.mapMaybe id .
  (HM.map (computePaths False)) .
  (HM.filter nonEmptyJ) $ obj

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

showj :: Value -> Text
showj Null = ""
showj (Bool b) = showt b
showj (Number n) = pack $ show n
showj (String s) = s