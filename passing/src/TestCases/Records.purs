module TestCases.Records (testRecords) where

import Effect
import Effect.Console
import Prelude
import PythonInterop

type IO = Effect


getFoo :: forall a r. {foo :: a | r} -> a
getFoo {foo} = foo

infixl 5 pyEq as ==
pyEq :: forall a b. a -> b -> Boolean
pyEq = unsafeEval "lambda a: lambda b: a == b"
    
testRecords :: IO Unit
testRecords = do
    flip assertMsg "invalid record 0" $ {foo: 1, bar: "2"} == unsafeEval "{'foo': 1, 'bar': '2'}"
    flip assertMsg "invalid record 1" $ {foo: "3", bar: 4} == unsafeEval "{'foo': '3', 'bar': 4}"
    flip assertMsg "invalid record 2" $ {foo: {}} == unsafeEval "{'foo': {}}"
    flip assertMsg "invalid record 3" $ getFoo {foo: {foo_nested: 5, bar_nested: 6}, bar: 7} == unsafeEval "{'foo_nested': 5, 'bar_nested': 6}"
