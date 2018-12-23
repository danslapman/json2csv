{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Lens ((^..))
import Control.Monad
import Control.Monad.Loops
import Data.Aeson
import Data.Aeson.Internal
import Data.Aeson.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (fold)
import Data.Function
import Data.List (union)
import Data.Maybe
import Data.Text (Text, intercalate)
import qualified Data.Text.IO as TIO
import Data.Traversable
import Json2Csv
import System.Environment
import System.IO

separator :: Text
separator = ";"

main :: IO ()
main = do
  (jsonFile : outFile : _) <- getArgs 
  contents <- BS.readFile $ jsonFile
  let lazyContents = LBS.fromStrict contents
  let parsed = decode lazyContents :: Maybe Value
  let (Just csv) = fmap convertSingle parsed
  bracket (openFile outFile WriteMode)
          hClose
          (\outHandle ->
            void $ traverse (TIO.hPutStrLn outHandle) csv)

convertSingle :: Value -> [Text]
convertSingle json = do
  let sepStr = intercalate separator
  let header = computePaths True json
  let entries = json ^.. values
  let makeLine val = sepStr $ fmap (showj . (fromMaybe Null) . ($ val) . (navigate)) header
  (sepStr $ fmap jsonPathText header) : (fmap makeLine entries)

computeHeaderMultiline :: Handle -> IO [JSONPath]
computeHeaderMultiline handle = do
  lines <- whileM (hIsEOF handle) $ do
      line <- fmap LBS.fromStrict $ BS.hGetLine handle
      let (Just parsed) = decode line :: Maybe Value
      return $ computePaths True parsed
  return $ foldl1 union lines