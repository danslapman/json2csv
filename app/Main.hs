{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.Function
import Json2Csv

--sut = decode "{\"foo\": [\"abc\",\"def\", {\"peka\": 1}]}" :: Maybe Value
sut = decode "[{\"peka\": 1}, {\"peka\": 2}, {\"peka\": 3}]" :: Maybe Value

main :: IO ()
main = print $ fmap (computeHeader True) sut
