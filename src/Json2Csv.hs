{-# LANGUAGE OverloadedStrings #-}

module Json2Csv (computeHeader) where

import Data.Aeson
import Data.Function
import Data.List.Index
import qualified Data.HashMap.Strict as HM
import Data.Text hiding (map, concat, concatMap)
import Data.Text.Buildable
import Data.Text.Format
import Data.Text.Lazy (toStrict)
import Data.Vector (toList)
import TextShow

appendPrefix :: Buildable b => Text -> [b] -> [Text]
appendPrefix prefix [] = [prefix]
appendPrefix prefix names = map (\name -> format "{}.{}" (prefix, name) & toStrict) names

computeHeader :: Bool -> Value -> [Text]
computeHeader _ Null = []
computeHeader _ (Bool _) = []
computeHeader _ (Number _) = []
computeHeader _ (String _) = []
computeHeader False (Array arr) =
  concat . (imap (\idx -> appendPrefix $ showt idx)) . (fmap (computeHeader False)) . toList $ arr
computeHeader True (Array arr) = concat . (fmap (computeHeader False)) . toList $ arr
computeHeader _ (Object obj) =
  concatMap (uncurry appendPrefix) . HM.toList . (HM.map (computeHeader False)) $ obj