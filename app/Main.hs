{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((^..))
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Function
import Data.Maybe
import Data.Text (Text, intercalate)
import Json2Csv
import System.Environment

separator :: Text
separator = ";"

main :: IO ()
main = do
  (jsonFile : _) <- getArgs 
  contents <- BS.readFile $ jsonFile
  let lazyContents = LBS.fromStrict contents
  let parsed = decode lazyContents :: Maybe Value
  print $ fmap convertSingle parsed

convertSingle :: Value -> [Text]
convertSingle json = do
  let sepStr = intercalate separator
  let header = computePaths True json
  let entries = json ^.. values
  let makeLine val = sepStr $ fmap (showj . (fromMaybe Null) . ($ val) . (navigate)) header
  (sepStr $ fmap jsonPathText header) : (fmap makeLine entries)