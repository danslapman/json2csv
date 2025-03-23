module HashSetUtilsSpec (hashSetTestCases) where

import qualified Data.HashMap.Strict as HM
import Data.HashSet
import HashSetUtils
import Test.HUnit

testMaybeNesEmptyHashSet :: Test
testMaybeNesEmptyHashSet = TestCase $ assertEqual "maybeNes empty HashSet" Nothing (maybeNes (empty :: HashSet Int))

testMaybeNesNonEmptyHashSet :: Test
testMaybeNesNonEmptyHashSet = TestCase $ assertEqual "maybeNes non-empty HashSet" (Just $ (fromList [1, 2, 3] :: HashSet Int)) (maybeNes $ fromList [1, 2, 3])

fromHashMapTest :: Test
fromHashMapTest = TestCase $ assertEqual "fromHashMap" (fromList [(1, "a"), (2, "b"), (3, "c")] :: HashSet (Int, String)) (fromHashMap $ HM.fromList [(1, "a"), (2, "b"), (3, "c")])

hashSetTestCases :: Test
hashSetTestCases =
  TestList
    [ testMaybeNesEmptyHashSet,
      testMaybeNesNonEmptyHashSet,
      fromHashMapTest
    ]