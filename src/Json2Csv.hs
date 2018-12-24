{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Json2Csv (computePaths, navigate, jsonPathText, showj) where

import Control.Lens ((^?), (^..))
import Control.Monad ((>=>))
import Data.Aeson
import Data.Aeson.Lens
import Data.Maybe (catMaybes)
import Data.Foldable (find)
import Data.List (union, null)
import Data.List.Index
import qualified Data.HashMap.Strict as HM
import Data.Semigroup
import Data.Text (Text, intercalate, pack)
import Data.Traversable
import Data.Typeable (Typeable)
import Data.Vector (toList)
import qualified Data.Vector as V
import Schema
import TextShow

concatUnion :: Eq a => [[a]] -> [a]
concatUnion = foldl1 union

prepend :: JsonPathElement -> [JsonPath] -> [JsonPath]
prepend prefix [] = [prefix : []]
prepend prefix path = fmap (\p -> prefix : p) path

nonEmptyJ :: Value -> Bool
nonEmptyJ Null = False
nonEmptyJ (Object o) | HM.null o = False
nonEmptyJ (Array a) | V.null a = False
nonEmptyJ _ = True

maybeNel :: [a] -> Maybe [a]
maybeNel = find (not . null) . Just

computePaths :: Bool -> Value -> Maybe [JsonPath]
computePaths _ Null = Just []
computePaths _ (Bool _) = Just []
computePaths _ (Number _) = Just []
computePaths _ (String _) = Just []
computePaths False (Array arr) =
  maybeNel .
  concat . 
  (imap (\_ -> prepend Iterator)) .
  catMaybes . 
  (fmap (computePaths False)) . 
  toList $ arr
computePaths True (Array arr) = 
  maybeNel . 
  concatUnion . 
  catMaybes . 
  (fmap (computePaths False)) . 
  toList $ arr
computePaths _ (Object obj) =
  maybeNel .
  concatMap (uncurry (\key -> (prepend (Key key)))) . 
  HM.toList .
  HM.mapMaybe id .
  (HM.map (computePaths False)) .
  (HM.filter nonEmptyJ) $ obj

(|=>) :: Monad f => (a -> f [b]) -> (b -> f [c]) -> a -> f [c]
(|=>) fx fy v = fmap concat (fx >=> (mapM fy) $ v) 

navigate :: JsonPath -> Value -> Maybe [Value]
navigate path =
  let stepFwd = \case
                  Key k -> fmap (:[]) . (^? key k) :: Value -> Maybe [Value]
                  Iterator -> maybeNel . (^.. values) :: Value -> Maybe [Value]
      (firstStep : otherSteps) = (fmap stepFwd path)
  in foldl (|=>) firstStep otherSteps

jsonPathText :: JsonPath -> Text
jsonPathText path =
  let repr = \case
               Key k -> k
               Iterator -> "$"
  in intercalate "." $ fmap repr path

showj :: Value -> Text
showj Null = ""
showj (Bool b) = showt b
showj (Number n) = pack $ show n
showj (String s) = s