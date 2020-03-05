module Test.Main where

import Prelude
import TestCases.Datatypes (testDatatypes)
import TestCases.PatternMaching (testPM)
import TestCases.Records (testRecords)

import Effect (Effect)
import Effect.Class.Console (log)

main :: Effect Unit
main = do
  testDatatypes
  testPM
  testRecords
  log "done"