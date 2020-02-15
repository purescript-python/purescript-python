{-# LANGUAGE GeneralizedNewtypeDeriving
#-}
module Monads.STEither where
import Control.Monad
import Control.Monad.Fail
import Control.Applicative
import Control.Monad.Except (MonadError)
import Control.Monad.State



newtype STEither r a b = STEither {runSTEither :: StateT r (Either a) b}
                         deriving (Monad, MonadError a)


instance Functor (STEither r a) where
    fmap = liftM

instance Applicative (STEither r a) where
    pure  = return
    (<*>) = ap