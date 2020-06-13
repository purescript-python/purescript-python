module Test.PyUtil where

import Prelude
import Effect (Effect)

foreign import setrecursionlimit :: Int -> Effect Unit
foreign import getrecursionlimit :: Effect Int
foreign import direct_print :: forall a. a -> Effect Unit