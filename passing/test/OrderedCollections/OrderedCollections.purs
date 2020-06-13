module Test.OrderedCollections where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Test.OrderedCollections.Data.Map (mapTests)
import Test.OrderedCollections.Data.Set (setTests)

testOrderedCollections :: Effect Unit
testOrderedCollections = do
  log "Running Map tests"
  mapTests
  log "Running Set tests"
  setTests
