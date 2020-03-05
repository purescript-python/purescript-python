module TestCases.Datatypes where

import Effect
import Prelude
import PythonInterop
import Effect.Console (log)


data MyEnum = A | B | C

-- ADT is abbr for algebraic data types
data MyADT -- this is the type declaration
    = ADT1 String
    | ADT2 Int Number
    | ADT3 { x :: Int, y :: Int }

newtype F = F MyADT

data MyParametricADT a -- this is the type declaration
    = PADT1 a
    | PADT2 Int Number
    | PADT3 { x :: Int, y :: Int, z :: a }

newtype PF a = PF {it :: MyParametricADT a}

testDatatypes :: Effect Unit
testDatatypes = do
    f <- eval "lambda x: set(x.keys()) == {'.t'}"
    assertMsg (f A) "invalid enumeration layout"
    assertMsg (f B) "invalid enumeration layout"
    assertMsg (f C) "invalid enumeration layout"

    ff <- eval "lambda name: lambda value: lambda x: x['.t'].__name__ == 'ps_' + name and {k: v for k, v in x.items() if k != '.t'} == value"
    
    f0 <- eval "lambda o: o['.t'].__name__ == 'ps_ADT1' and o['value0'] == 'this is a string'"
    assertMsg (f0 $ ADT1 "this is a string") "invalid ADT layout 1"
    f1 <- eval "lambda o: o['.t'].__name__ == 'ps_ADT2' and o['value0'] == 1 and o['value1'] == 2.0 and (type(o['value1']), type(o['value0'])) == (float, int)"
    assertMsg (f1 $ ADT2 1 2.0) "invalid ADT layout 2"
    
    f2 <- eval "lambda o: o['value0']['x'] == 22 and o['value0']['y'] == 33" 
    assertMsg (f2 $ ADT3 {x : 22, y : 33}) "invalid ADT layout 3"

    (any_eq :: forall a. a) <- eval "lambda a: lambda b: a == b"
    assertMsg (F (ADT3 {x : 22, y : 33}) `any_eq` (ADT3 {x : 22, y : 33})) "invalid newtype layout"

    f3 <- eval "lambda o: o['value0'] == 'this is a string'"
    assertMsg (f3 $ PADT1 "this is a string") "invalid parameteric ADT 1"
    
    f4 <- eval "lambda o: (o['value0'], o['value1']) == (1, 2.0) and (type(o['value0']), type(o['value1'])) == (int, float)"

    assertMsg (f4 $ PADT2 1 2.0) "invalid parameteric ADT 2"
    unwrap <- eval "lambda x: x['value0']"
    assertMsg (unwrap (PADT3 {x : 22, y : 33, z : 2}) `any_eq` {x : 22, y : 33, z: 2}) "invalid parameteric ADT 3"
    
    let o = PF {it : PADT3 {x : 22, y : 33, z: 2}}
    flip assertMsg "invalid nested box" $ case o of
        PF {it : PADT3 {x, y}} | x `any_eq` 22 && y `any_eq` 33 -> true
        _ -> false
