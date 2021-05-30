{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UntypedPremonoidal.Name where

import Control.Monad.Trans.State (StateT, evalStateT, get, put)
import Data.Functor.Identity (Identity, runIdentity)


newtype Name = Name Int
  deriving (Eq, Ord)

newtype NameGenT m a = NameGenT
  { unNameGenT :: StateT Int m a }
  deriving (Functor, Applicative, Monad)

runNameGenT
  :: Monad m
  => NameGenT m a
  -> m a
runNameGenT
  = flip evalStateT 0
  . unNameGenT

genName
  :: Monad m
  => NameGenT m Name
genName = NameGenT $ do
  n <- get
  put (n+1)
  pure $ Name (n+1)


type NameGen = NameGenT Identity

runNameGen
  :: NameGen a -> a
runNameGen
  = runIdentity
  . runNameGenT
