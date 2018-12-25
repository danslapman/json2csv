{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Lens ((^..))
import Control.Monad hiding (forM_)
import Control.Monad.Loops
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Function
import Data.Maybe
import Data.Text (Text, intercalate)
import qualified Data.Text.IO as TIO
import Data.Traversable
import Data.Vector hiding (sequence)
import Json2Csv
import Prelude hiding (foldl, sequence)
import Schema
import System.Environment
import System.IO
import Util

separator :: Text
separator = ";"

mkSepString :: Vector Text -> Text
mkSepString = intercalate separator . toList

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

computeHeaderMultiline :: Handle -> IO (Vector JsonPath)
computeHeaderMultiline handle = do
  lines <- whileM (fmap not $ hIsEOF handle) $ do
      line <- fmap LBS.fromStrict $ BS.hGetLine handle
      let (Just parsed) = decode line :: Maybe Value
      let (Just header) = computePaths True parsed
      return $ header
  return $ foldl vunion empty $ fromList lines

parseAndWriteEntry :: (Vector JsonPath) -> Handle -> Handle -> IO ()
parseAndWriteEntry header hIn hOut = do
  line <- fmap LBS.fromStrict $ BS.hGetLine hIn
  let schema = toSchema header
  let (Just parsed) = decode line :: Maybe Value
  let tree = extract schema parsed
  print schema
  print tree
  print $ generateTuples tree
  -- let lines = sequence $ fmap ((fromMaybe (singleton Null)) . ($ parsed) . (navigate)) header
  -- forM_ lines $ \line -> TIO.hPutStrLn hOut $ mkSepString . (fmap showj) $ line