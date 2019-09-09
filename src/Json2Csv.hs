{-# LANGUAGE OverloadedStrings #-}

module Json2Csv (computePaths, showj) where

import Control.Applicative (pure)
import Data.Aeson
import Data.Maybe (mapMaybe)
import qualified Data.HashMap.Strict as HM
import Data.HashSet
import Data.Text (Text, pack)
import qualified Data.Vector as V
import Deque (cons)
import Prelude hiding (concatMap, foldl, join, map, null)
import HashSetUtils
import Schema
import TextShow hiding (singleton)

prepend :: JsonPathElement -> HashSet JsonPath -> HashSet JsonPath
prepend prefix s | null s = singleton $ pure prefix
prepend prefix path = map (prefix `cons`) path

nonEmptyJ :: Value -> Bool
nonEmptyJ Null = False
nonEmptyJ (Object o) | HM.null o = False
nonEmptyJ (Array a) | V.null a = False
nonEmptyJ _ = True

computePaths :: Bool -> Value -> Maybe (HashSet JsonPath)
computePaths _ Null = Just empty
computePaths _ (Bool _) = Just empty
computePaths _ (Number _) = Just empty
computePaths _ (String _) = Just empty
computePaths False (Array arr) =
  maybeNes .
  unions .
  (fmap (prepend Iterator)) .
  (mapMaybe id) . 
  (fmap (computePaths False)) .
  V.toList $ arr
computePaths True (Array arr) = 
  maybeNes .
  unions .
  (mapMaybe id) . 
  (fmap (computePaths False)) . 
  V.toList $ arr
computePaths _ (Object obj) =
  maybeNes .
  unions .
  (uncurry (prepend . Key) <$>) .
  HM.toList .
  HM.mapMaybe id .
  (HM.map (computePaths False)) .
  (HM.filter nonEmptyJ) $ obj 

showj :: Value -> Text
showj Null = ""
showj (Bool b) = showt b
showj (Number n) = pack $ show n
showj (String s) = s