module Test.Arrays where

import Prelude
import Effect (Effect)
import Test.Arrays.Data.Array (testArray)
import Test.PyUtil (setrecursionlimit, getrecursionlimit)
import Effect.Class.Console (log)
import Test.Arrays.Data.Array.Partial (testArrayPartial)
import Test.Arrays.Data.Array.ST (testArrayST)
import Test.Arrays.Data.Array.ST.Partial (testArraySTPartial)
import Test.Arrays.Data.Array.NonEmpty (testNonEmptyArray)

testArrays :: Effect Unit
testArrays = do
  testArray
  testArrayST
  testArrayPartial
  testArraySTPartial
  testNonEmptyArray
