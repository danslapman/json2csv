{-# LANGUAGE OverloadedStrings #-}

module Main where

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
import Data.Set.Monad (Set, unions)
import qualified Data.Set.Monad as SM (toList)
import Data.Text (Text, intercalate)
import qualified Data.Text.IO as TIO
import Deque
import DequeUtils
import Json2Csv
import Prelude hiding (foldl, foldl', sequence)
import Schema
import System.Environment
import System.IO

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
      whileM_ (not <$> hIsEOF hIn) (parseAndWriteEntry schema columns hIn hOut)

computeHeaderMultiline :: Handle -> IO (Deque JsonPath)
computeHeaderMultiline handle = do
  lineNnumber <- newIORef (0 :: Int)
  lines <- whileM (not <$> hIsEOF handle) $ do
      modifyIORef lineNnumber (1+)
      line <- LBS.fromStrict <$> BS.hGetLine handle
      ln <- readIORef lineNnumber
      parsed <- case eitherDecode' line of
                         Right value -> pure value
                         Left err -> fail $ "Can't parse JSON at line " ++ (show ln) ++ ": " ++ err
      let (Just header) = computePaths True parsed
      return $ header
  return $ fromList . SM.toList . unions $ lines

parseAndWriteEntry :: JsonSchema -> Deque Text -> Handle -> Handle -> IO ()
parseAndWriteEntry schema columns hIn hOut = do
  line <- LBS.fromStrict <$> BS.hGetLine hIn
  let (Just parsed) = decode line :: Maybe Value
  let tree = extract schema parsed
  let tuples = generateTuples tree
  let lines = ((<$> columns) . flip (HM.lookupDefault Null)) <$> tuples
  forM_ lines (TIO.hPutStrLn hOut . mkSepString . fmap showj)