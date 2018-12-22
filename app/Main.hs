{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.Function
import Json2Csv

sut = decode "{\"foo\": [\"abc\",\"def\", {\"peka\": 1}]}" :: Maybe Value

main :: IO ()
main = print $ fmap (computeHeader True) sut
