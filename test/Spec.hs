import HashSetUtilsSpec
import SequenceUtilsSpec
import Test.HUnit
import System.Exit (exitFailure)

processCounts :: Counts -> IO ()
processCounts counts = do
  if errors counts + failures counts == 0
  then return ()
  else exitFailure

main :: IO ()
main = do
  hsCounts <- runTestTT hashSetTestCases
  processCounts hsCounts
  seqCounts <- runTestTT seqTestCases
  processCounts seqCounts