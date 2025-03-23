module SequenceUtilsSpec (seqTestCases) where

import Data.Sequence
import SequenceUtils
import Test.HUnit

testMaybeNesEmptySequence :: Test
testMaybeNesEmptySequence = TestCase $ assertEqual "maybeNes empty sequence" Nothing (maybeNes (empty :: Seq Int))

testMaybeNesNonEmptySequence :: Test
testMaybeNesNonEmptySequence = TestCase $ assertEqual "maybeNes non-empty sequence" (Just $ fromList [1, 2, 3]) (maybeNes $ fromList [1, 2, 3])

testUniqEmptySequence :: Test
testUniqEmptySequence = TestCase $ assertEqual "uniq empty sequence" (empty :: Seq Int) (uniq (empty :: Seq Int))

testUniqNonEmptySequence :: Test
testUniqNonEmptySequence = TestCase $ assertEqual "uniq non-empty sequence" (fromList [1, 2, 3]) (uniq $ fromList [1, 2, 2, 3])

testMapMaybeEmptySequence :: Test
testMapMaybeEmptySequence = TestCase $ assertEqual "mapMaybe empty sequence" (empty :: Seq Int) (mapMaybe (const Nothing) (empty :: Seq Int))

testMapMaybeNonEmptySequence :: Test
testMapMaybeNonEmptySequence = TestCase $ assertEqual "mapMaybe non-empty sequence" (fromList [1, 3]) (mapMaybe (\x -> if x == 2 then Nothing else Just x) $ fromList [1, 2, 3])

seqTestCases :: Test
seqTestCases =
  TestList
    [ testMaybeNesEmptySequence,
      testMaybeNesNonEmptySequence,
      testUniqEmptySequence,
      testUniqNonEmptySequence,
      testMapMaybeEmptySequence,
      testMapMaybeNonEmptySequence
    ]