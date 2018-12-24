{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Lens ((^..))
import Control.Monad
import Control.Monad.Loops
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Function
import Data.List (union)
import Data.Maybe
import Data.Text (Text, intercalate)
import qualified Data.Text.IO as TIO
import Data.Traversable
import Json2Csv
import Schema
import System.Environment
import System.IO

separator :: Text
separator = ";"

mkSepString :: [Text] -> Text
mkSepString = intercalate separator

main :: IO ()
main = do
  (jsonFile : outFile : _) <- getArgs
  header <- withFile jsonFile ReadMode computeHeaderMultiline
  withFile jsonFile ReadMode $ \hIn ->
    withFile outFile WriteMode $ \hOut -> do
      hSetEncoding hIn utf8
      hSetEncoding hOut utf8
      TIO.hPutStrLn hOut $ mkSepString $ fmap jsonPathText header
      whileM_ (fmap not $ hIsEOF hIn) (parseAndWriteEntry header hIn hOut)

computeHeaderMultiline :: Handle -> IO [JsonPath]
computeHeaderMultiline handle = do
  lines <- whileM (fmap not $ hIsEOF handle) $ do
      line <- fmap LBS.fromStrict $ BS.hGetLine handle
      let (Just parsed) = decode line :: Maybe Value
      let (Just header) = computePaths True parsed
      return $ header
  return $ foldl union [] lines

parseAndWriteEntry :: [JsonPath] -> Handle -> Handle -> IO ()
parseAndWriteEntry header hIn hOut = do
  line <- fmap LBS.fromStrict $ BS.hGetLine hIn
  let (Just parsed) = decode line :: Maybe Value
  let lines = sequence $ fmap ((fromMaybe [Null]) . ($ parsed) . (navigate)) header
  forM_ lines $ \line -> TIO.hPutStrLn hOut $ mkSepString . (fmap showj) $ line