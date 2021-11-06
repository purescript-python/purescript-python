module Main(e) where

import Diana (unit, Unit, log)


e :: Unit
e = unit

discard :: Unit -> Unit
discard _ = unit

ignore :: forall a. a -> Unit
ignore _ = unit

main :: Unit
main =
    let _ = ignore (log "🍝") in
    let z = 1 in
    log z

