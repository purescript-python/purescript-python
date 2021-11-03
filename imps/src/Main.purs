module Main where

data Unit = Unit
foreign import log :: forall a. a -> Unit

main :: Unit
main = log "🍝"
