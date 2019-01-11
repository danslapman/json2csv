{-# LANGUAGE OverloadedStrings #-}

module Json2Csv (computePaths, showj) where

import Control.Applicative (pure)
import Control.Monad (join)
import Data.Foldable (foldl1)
import qualified Data.List as L
import Data.Maybe (catMaybes)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, pack)
import Data.Typeable (Typeable)
import qualified Data.Vector as V
import Deque hiding (prepend)
import DequePatterns
import DequeUtils hiding (null)
import Prelude hiding (concatMap, foldl, join, null)
import Schema
import Text.JSON
import TextShow hiding (singleton)

prepend :: JsonPathElement -> Deque JsonPath -> Deque JsonPath
prepend prefix D_ = pure $ pure prefix
prepend prefix path = (prefix `cons`) <$> path

nonEmptyJ :: JSValue -> Bool
nonEmptyJ JSNull = False
nonEmptyJ (JSObject o) | L.null $ fromJSObject o = False
nonEmptyJ (JSArray a) | L.null a = False
nonEmptyJ _ = True

computePaths :: Bool -> JSValue -> Maybe (Deque JsonPath)
computePaths _ JSNull = Just empty
computePaths _ (JSBool _) = Just empty
computePaths _ (JSRational _ _) = Just empty
computePaths _ (JSString _) = Just empty
computePaths False (JSArray arr) =
  maybeNeq .
  join .
  (fmap (prepend Iterator)) .
  (mapMaybe id) . 
  (fmap (computePaths False)) .
  fromList $ arr
computePaths True (JSArray arr) = 
  maybeNeq .
  foldl1 union .
  (mapMaybe id) . 
  (fmap (computePaths False)) .
  fromList $ arr
computePaths _ (JSObject obj) =
  maybeNeq .
  (uncurry (prepend . Key) =<<) .
  hmToDeque .
  HM.mapMaybe id .
  (HM.map (computePaths False)) .
  (HM.filter nonEmptyJ) .
  HM.fromList .
  fmap (\(k,v) -> (pack k, v)) .
  fromJSObject $ obj 

showj :: JSValue -> Text
showj JSNull = ""
showj (JSBool b) = showt b
showj (JSRational _ n) = pack $ show n
showj (JSString s) = pack $ fromJSString s