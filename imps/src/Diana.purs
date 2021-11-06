module Diana
    (Unit(..), unit, ($), call, require, __file__, log)
where

foreign import data Unit :: Type

foreign import require :: forall a. String -> a
foreign import __file__ :: String
foreign import log :: forall a . a -> Unit


ffiModule :: { none :: Unit, unsafe_getitem :: forall a b. a -> String -> b, unsafe_setitem :: forall a b. a -> String -> b -> Unit }
ffiModule = require("./@ffi.diana")

unit :: Unit
unit = ffiModule.none


infixr 0 call as $
call :: forall a b. (a -> b) -> a -> b
call f x = f x

class Number a where
    add :: a -> a -> a
    sub :: a -> a -> a
    div :: a -> a -> a    
    mul :: a -> a -> a
    mod :: a -> a -> a


class Sign a where
    neg :: a -> a

