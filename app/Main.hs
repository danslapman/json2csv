{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Monad
import Control.Monad.Loops
import Data.Either
import Data.Foldable (foldl, toList)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Text (Text, intercalate)
import qualified Data.Text.IO as TIO
import Deque
import DequeUtils
import Json2Csv
import Prelude hiding (foldl, sequence)
import Schema
import System.Environment
import System.IO
import Text.JSON

separator :: Text
separator = ";"

mkSepString :: Deque Text -> Text
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

computeHeaderMultiline :: Handle -> IO (Deque JsonPath)
computeHeaderMultiline handle = do
  lineNnumber <- newIORef (0 :: Int)
  lines <- whileM (fmap not $ hIsEOF handle) $ do
      modifyIORef lineNnumber (1+)
      line <- hGetLine handle
      ln <- readIORef lineNnumber
      parsed <- case decode line of
                         Ok value -> pure value
                         Error err -> fail $ "Can't parse JSON at line " ++ (show ln) ++ ": " ++ err
      let (Just header) = computePaths True parsed
      return $ header
  return $ foldl union empty $ fromList lines

parseAndWriteEntry :: JsonSchema -> Deque Text -> Handle -> Handle -> IO ()
parseAndWriteEntry schema columns hIn hOut = do
  line <- hGetLine hIn
  let (Ok parsed) = decode line
  let tree = extract schema parsed
  let tuples = generateTuples tree
  let lines = ((<$> columns) . flip (HM.lookupDefault JSNull)) <$> tuples
  forM_ lines (TIO.hPutStrLn hOut . mkSepString . fmap showj)