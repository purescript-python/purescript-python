module Test.PyUtil where

import Prelude
import Effect (Effect)

foreign import setrecursionlimit :: Int -> Effect Unit
foreign import getrecursionlimit :: Effect Int
