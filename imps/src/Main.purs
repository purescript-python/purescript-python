module Main where

import Prelude
import Effect

import Effect.Class
import Effect.Console

data Unit2 = Unit2

xxx :: Effect Int
xxx = do
    log "5"
    pure $ 1 + 10

main :: Effect Unit
main = do
    log "🍝"
    z <- xxx
    log $ show (z * 3)
