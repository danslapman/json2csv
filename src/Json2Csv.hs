{-# LANGUAGE OverloadedStrings #-}

module Json2Csv (computePaths) where

--import Control.Lens ((^?))
import Data.Aeson
import Data.Aeson.Internal
import Data.List (union)
import Data.List.Index
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import Data.Vector (toList)

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