module TestCases.PatternMaching where

import Prelude
import PythonInterop

import Data.Function (applyN)
import Effect (Effect)

data Nat
  = Zero       -- 0
  | Succ Nat   -- succ 0 -> 1; succ 2 -> 3

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ a) = 1 + natToInt a

natToIntRecImpl :: Int -> Nat -> Int
natToIntRecImpl a Zero = a
natToIntRecImpl r (Succ a) = natToIntRecImpl (1 + r) a

natToIntRec :: _
natToIntRec = natToIntRecImpl 0

intToNatRecImpl :: Nat -> Int -> Nat
intToNatRecImpl a 0 = a
intToNatRecImpl a n = intToNatRecImpl (Succ a) (n + -1)

intToNatRec :: _
intToNatRec n
  | n < 0        = error ""
  | true         = intToNatRecImpl Zero n

_3 :: Nat
_3 = Succ (Succ (Succ Zero))


testDeconsGuard :: _
-- test deconstructions in guards
testDeconsGuard n
  | Succ a <- n = false
  | true = true

testPM :: Effect Unit
testPM = do
    let (any_eq :: forall a b. a -> b -> Boolean) = unsafeEval "lambda a: lambda b: a == b"
    assertMsg (natToInt _3 `any_eq` 3) "invalid construction(non rec)"
    assertMsg (natToIntRec _3 `any_eq` 3) "invalid construction(rec) 0"
    assertMsg (10000 `any_eq` natToIntRec (intToNatRec 10000)) "invalid construction(rec) 1"
    assertMsg (false `any_eq` testDeconsGuard _3) "invalid deconstruction 0"
    assertMsg (true `any_eq` testDeconsGuard Zero) "invalid deconstruction 1"
