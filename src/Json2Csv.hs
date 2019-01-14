{-# LANGUAGE OverloadedStrings #-}

module Json2Csv (computePaths, showj) where

import Control.Applicative (pure)
import Control.Monad (join)
import Data.Aeson
import Data.Foldable (foldl1)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.HashMap.Strict as HM
import Data.Set.Monad
import Data.Text (Text, pack)
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import Deque (cons)
import Prelude hiding (concatMap, foldl, join, null)
import SetUtils
import Schema
import TextShow hiding (singleton)

prepend :: JsonPathElement -> Set JsonPath -> Set JsonPath
prepend prefix s | null s = pure $ pure prefix
prepend prefix path = (prefix `cons`) <$> path

nonEmptyJ :: Value -> Bool
nonEmptyJ Null = False
nonEmptyJ (Object o) | HM.null o = False
nonEmptyJ (Array a) | V.null a = False
nonEmptyJ _ = True

computePaths :: Bool -> Value -> Maybe (Set JsonPath)
computePaths _ Null = Just empty
computePaths _ (Bool _) = Just empty
computePaths _ (Number _) = Just empty
computePaths _ (String _) = Just empty
computePaths False (Array arr) =
  maybeNes .
  join .
  fromList .
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
  (uncurry (prepend . Key) =<<) .
  fromHashMap .
  HM.mapMaybe id .
  (HM.map (computePaths False)) .
  (HM.filter nonEmptyJ) $ obj 

showj :: Value -> Text
showj Null = ""
showj (Bool b) = showt b
showj (Number n) = pack $ show n
showj (String s) = s