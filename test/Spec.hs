import SequenceUtilsSpec
import Test.HUnit
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
  counts <- runTestTT seqTestCases
  if errors counts + failures counts == 0
        then exitSuccess
        else exitFailure