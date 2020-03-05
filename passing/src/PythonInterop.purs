module PythonInterop where
import Prelude
import Effect (Effect)

-- | Equal to `assert cond, msg` in Python but wrapped in Effect monad.
foreign import assertMsg :: Boolean -> String -> Effect Unit

-- | Equal to `assert cond` in Python but wrapped in Effect monad.
foreign import assert_ :: Boolean -> Effect Unit

-- | Equal to `repr` in Python but wrapped in Effect monad.
foreign import repr :: forall a. a -> Effect String

-- | Equal to `eval` in Python but wrapped in Effect monad.
foreign import eval :: forall a. String -> Effect a

-- | Equal to `eval` in Python but wrapped in Effect monad.
foreign import unsafeEval :: forall a. String -> a

-- | Equal to `eval` in Python but wrapped in Effect monad.
foreign import error :: forall a. String -> a

assert :: Boolean -> Effect Unit
assert = assert_