{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Json2Csv (computePaths, navigate, jsonPathText, showj) where

import Control.Lens ((^?), (^..))
import Data.Aeson
import Data.Aeson.Lens
import Data.Maybe (catMaybes)
import qualified Data.HashMap.Strict as HM
import Data.Semigroup
import Data.Text (Text, intercalate, pack)
import Data.Traversable
import Data.Typeable (Typeable)
import Data.Vector
import Prelude hiding (concatMap, foldl, null)
import Schema
import TextShow hiding (singleton)
import Util
import VecPats

prepend :: JsonPathElement -> Vector JsonPath -> Vector JsonPath
prepend prefix V_ = singleton $ singleton prefix
prepend prefix path = fmap (\p -> prefix `cons` p) path

nonEmptyJ :: Value -> Bool
nonEmptyJ Null = False
nonEmptyJ (Object o) | HM.null o = False
nonEmptyJ (Array a) | null a = False
nonEmptyJ _ = True

computePaths :: Bool -> Value -> Maybe (Vector JsonPath)
computePaths _ Null = Just empty
computePaths _ (Bool _) = Just empty
computePaths _ (Number _) = Just empty
computePaths _ (String _) = Just empty
computePaths False (Array arr) =
  maybeNev .
  vconcat . 
  (imap (\_ -> prepend Iterator)) .
  (mapMaybe id) . 
  (fmap (computePaths False)) $ arr
computePaths True (Array arr) = 
  maybeNev . 
  concatUnion . 
  (mapMaybe id) . 
  (fmap (computePaths False)) $ arr
computePaths _ (Object obj) =
  maybeNev .
  concatMap (uncurry (\key -> (prepend (Key key)))) . 
  hmToVector .
  HM.mapMaybe id .
  (HM.map (computePaths False)) .
  (HM.filter nonEmptyJ) $ obj 

navigate :: JsonPath -> Value -> Maybe (Vector Value)
navigate path =
  let stepFwd = \case
                  Key k -> fmap singleton . (^? key k) :: Value -> Maybe (Vector Value)
                  Iterator -> maybeNev . fromList . (^.. values) :: Value -> Maybe (Vector Value)
      (firstStep ::: otherSteps) = (fmap stepFwd path)
  in foldl (|=>) firstStep otherSteps

jsonPathText :: JsonPath -> Text
jsonPathText path =
  let repr = \case
               Key k -> k
               Iterator -> "$"
  in intercalate "." $ toList $ fmap repr path

showj :: Value -> Text
showj Null = ""
showj (Bool b) = showt b
showj (Number n) = pack $ show n
showj (String s) = s