module Diana
    (Unit(..), unit, ($), call, log)
where

foreign import data Unit :: Type
foreign import require :: forall a. String -> a
foreign import unit :: Unit
foreign import log :: forall a . a -> Unit


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

