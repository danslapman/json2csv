{-# LANGUAGE OverloadedStrings #-}

module Json2Csv (computePaths, showj) where

import Control.Applicative (pure)
import Control.Lens ((^?), (^..))
import Control.Monad (join)
import Data.Aeson
import Data.Aeson.Lens
import Data.Foldable (concatMap, foldl1)
import Data.Maybe (catMaybes)
import qualified Data.HashMap.Strict as HM
import Data.Semigroup
import Data.Text (Text, intercalate, pack)
import Data.Traversable
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import Deque hiding (prepend)
import DequePatterns
import DequeUtils hiding (null)
import Prelude hiding (concatMap, foldl, join, null)
import Schema
import TextShow hiding (singleton)

prepend :: JsonPathElement -> Deque JsonPath -> Deque JsonPath
prepend prefix D_ = pure $ pure prefix
prepend prefix path = (prefix `cons`) <$> path

nonEmptyJ :: Value -> Bool
nonEmptyJ Null = False
nonEmptyJ (Object o) | HM.null o = False
nonEmptyJ (Array a) | V.null a = False
nonEmptyJ _ = True

computePaths :: Bool -> Value -> Maybe (Deque JsonPath)
computePaths _ Null = Just empty
computePaths _ (Bool _) = Just empty
computePaths _ (Number _) = Just empty
computePaths _ (String _) = Just empty
computePaths False (Array arr) =
  maybeNeq .
  join .
  (fmap (prepend Iterator)) .
  (mapMaybe id) . 
  (fmap (computePaths False)) .
  fromVector $ arr
computePaths True (Array arr) = 
  maybeNeq .
  foldl1 union .
  (mapMaybe id) . 
  (fmap (computePaths False)) .
  fromVector $ arr
computePaths _ (Object obj) =
  maybeNeq .
  (uncurry (prepend . Key) =<<) .
  hmToDeque .
  HM.mapMaybe id .
  (HM.map (computePaths False)) .
  (HM.filter nonEmptyJ) $ obj 

showj :: Value -> Text
showj Null = ""
showj (Bool b) = showt b
showj (Number n) = pack $ show n
showj (String s) = s