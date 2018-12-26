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
import qualified Data.HashMap.Strict as HM
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
  let schema = toSchema header
  let columns = jsonPathText <$> header
  withFile jsonFile ReadMode $ \hIn ->
    withFile outFile WriteMode $ \hOut -> do
      hSetEncoding hIn utf8
      hSetEncoding hOut utf8
      TIO.hPutStrLn hOut $ mkSepString $ columns
      whileM_ (fmap not $ hIsEOF hIn) (parseAndWriteEntry schema columns hIn hOut)

computeHeaderMultiline :: Handle -> IO (Vector JsonPath)
computeHeaderMultiline handle = do
  lines <- whileM (fmap not $ hIsEOF handle) $ do
      line <- fmap LBS.fromStrict $ BS.hGetLine handle
      let (Just parsed) = decode line :: Maybe Value
      let (Just header) = computePaths True parsed
      return $ header
  return $ foldl vunion empty $ fromList lines

parseAndWriteEntry :: JsonSchema -> Vector Text -> Handle -> Handle -> IO ()
parseAndWriteEntry schema columns hIn hOut = do
  line <- fmap LBS.fromStrict $ BS.hGetLine hIn
  let (Just parsed) = decode line :: Maybe Value
  let tree = extract schema parsed
  let tuples = generateTuples tree
  let lines = ((<$> columns) . flip (HM.lookupDefault Null)) <$> tuples
  forM_ lines (TIO.hPutStrLn hOut . mkSepString . fmap showj)