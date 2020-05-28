module Test.Arrays where

import Prelude
import Effect (Effect)
import Test.Arrays.Data.Array (testArray)
import Test.PyUtil (setrecursionlimit, foo)
import Effect.Class.Console (log)
-- import Test.Arrays.Data.Array.Partial (testArrayPartial)
-- import Test.Arrays.Data.Array.ST (testArrayST)
-- import Test.Arrays.Data.Array.ST.Partial (testArraySTPartial)
-- import Test.Arrays.Data.Array.NonEmpty (testNonEmptyArray)

testArrays :: Effect Unit
testArrays = do
    log $ show $ foo 1 2 
--   setrecursionlimit 100000
--   testArray
--   testArrayST
--   testArrayPartial
--   testArraySTPartial
--   testNonEmptyArray
