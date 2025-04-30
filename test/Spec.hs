import HashSetUtilsSpec
import SequenceUtilsSpec
import Test.HUnit

main :: IO ()
main = do
  _ <- runTestTT hashSetTestCases
  _ <- runTestTT seqTestCases
  return ()