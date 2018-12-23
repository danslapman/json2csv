{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.ByteString as BS
import Data.ByteString.Lazy as LBS
import Data.Function
import Json2Csv
import System.Environment

main :: IO ()
main = do
  (jsonFile : _) <- getArgs 
  contents <- BS.readFile $ jsonFile
  let lazyContents = LBS.fromStrict contents
  let parsed = decode lazyContents :: Maybe Value
  print $ fmap (computePaths True) parsed
