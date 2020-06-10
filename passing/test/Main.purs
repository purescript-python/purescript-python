module Test.Main where

import Prelude
import TestCases.Datatypes (testDatatypes)
import TestCases.PatternMaching (testPM)
import TestCases.Records (testRecords)
import Effect (Effect)
import Effect.Class.Console (log)
import Test.Unfoldable (testUnfoldable)
import Test.ST (testST)
import Test.UnsafeCoerce (testUnsafeCoerce)
import Test.Arrays (testArrays)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Test.OrderedCollections (testOrderedCollections)
import Test.Int (testInt)
import Test.Record (testRecord)
import Test.Globals (testGlobals)
import Global as G
import Global.Unsafe as GU
import Record as R
import Test.PyUtil
import Test.String (testStringAll)
import Test.TestQuickCheck (testQuickCheck)

main :: Effect Unit
main = do
  testDatatypes
  testPM
  testRecords
  testRecord
  testUnfoldable
  testST
  testUnsafeCoerce
  testArrays
  testInt
  testGlobals
  testStringAll
--   testQuickCheck
--   testOrderedCollections
--   log "CI tests passing!"
