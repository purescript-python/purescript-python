module Test.ST where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)
import Control.Monad.ST as ST
import Control.Monad.ST.Ref as STRef

sumOfSquares :: Int
sumOfSquares =
  ST.run do
    total <- STRef.new 0
    let
      loop 0 = STRef.read total

      loop n = do
        _ <- STRef.modify (_ + (n * n)) total
        loop (n - 1)
    loop 100

testST :: Effect Unit
testST = do
  logShow sumOfSquares
