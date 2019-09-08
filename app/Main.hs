{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Loops
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Either
import Data.Foldable (foldl', toList)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.HashSet (HashSet, empty, intersection, null, union, unions)
import qualified Data.HashSet as HS (toList)
import Data.Text (Text, intercalate)
import qualified Data.Text.IO as TIO
import Deque (Deque, fromList)
import DequeUtils hiding (empty, union)
import Json2Csv
import Options.Applicative hiding (empty)
import Options.Applicative.Text
import Prelude hiding (foldl, foldl', null, sequence)
import Schema
import System.Environment
import System.IO

data Args = Args {
  jsonFile :: String,
  csvFile :: String,
  separator :: Text,
  isect :: Bool
}

type PathSet = HashSet JsonPath
type PathSetCombine = PathSet -> PathSet -> PathSet

args :: Parser Args
args = Args 
  <$> strArgument (metavar "jsonFile" <> help "Newline-delimited JSON input file name")
  <*> strArgument (metavar "csvFile" <> help "CSV output file name")
  <*> textOption (long "separator" <> help "CSV separator" <> showDefault <> value ";")
  <*> switch (long "intersect" <> short 'i' <> help "\"Inner join\" fields while constructing schema")

argsInfo :: ParserInfo Args
argsInfo = info args fullDesc

isectOrNonEmpty :: PathSetCombine
isectOrNonEmpty s1 s2 | null s1 = s2
isectOrNonEmpty s1 s2 | null s2 = s1
isectOrNonEmpty s1 s2 = intersection s1 s2

main :: IO ()
main = do
  arguments <- execParser argsInfo
  let mkSepString = intercalate (separator arguments) . toList
  let combine = if (isect arguments) then isectOrNonEmpty else flip union
  header <- withFile (jsonFile arguments) ReadMode $ computeHeaderMultiline combine
  let schema = toSchema header
  let columns = jsonPathText <$> header
  withFile (jsonFile arguments) ReadMode $ \hIn ->
    withFile (csvFile arguments) WriteMode $ \hOut -> do
      hSetEncoding hIn utf8
      hSetEncoding hOut utf8
      TIO.hPutStrLn hOut $ mkSepString $ columns
      whileM_ (not <$> hIsEOF hIn) (parseAndWriteEntry mkSepString schema columns hIn hOut)

computeHeaderMultiline :: PathSetCombine -> Handle -> IO (Deque JsonPath)
computeHeaderMultiline combine handle = do
  currentLineNumber <- newIORef (0 :: Int)
  pathSet <- newIORef (empty :: HashSet JsonPath)
  whileM_ (not <$> hIsEOF handle) $ do
      modifyIORef' currentLineNumber (1+)
      line <- LBS.fromStrict <$> BS.hGetLine handle
      ln <- readIORef currentLineNumber
      parsed <- case eitherDecode' line of
                         Right value -> pure value
                         Left err -> fail $ "Can't parse JSON at line " ++ (show ln) ++ ": " ++ err
      let (Just header) = computePaths True parsed
      modifyIORef' pathSet (combine $!! header)
  pathes <- readIORef pathSet
  return $ fromList . HS.toList $ pathes

parseAndWriteEntry :: (Deque Text -> Text) -> JsonSchema -> Deque Text -> Handle -> Handle -> IO ()
parseAndWriteEntry mkSepString schema columns hIn hOut = do
  line <- LBS.fromStrict <$> BS.hGetLine hIn
  let (Just parsed) = decode line :: Maybe Value
  let tree = extract schema parsed
  let tuples = generateTuples tree
  let lines = ((<$> columns) . flip (HM.lookupDefault Null)) <$> tuples
  forM_ lines (TIO.hPutStrLn hOut . mkSepString . fmap showj)