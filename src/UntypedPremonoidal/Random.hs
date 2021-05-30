{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UntypedPremonoidal.Random where

import Control.Monad (replicateM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, get, put)
import System.Random (randomRIO)

import UntypedPremonoidal.KnownSize


newtype Random a = Random
  { unRandom :: StateT [String] IO a }
  deriving (Functor, Applicative, Monad)

runRandom
  :: Random a
  -> IO a
runRandom
  = flip evalStateT names
  . unRandom
  where
    letters :: [Char]
    letters
      = ['f'..'z']
     ++ ['a'..'e']

    names :: [String]
    names = do
      n <- [1..]
      replicateM n letters

pickFrom
  :: [a]
  -> Random a
pickFrom xs = Random $ do
  i <- lift $ randomRIO (0, length xs - 1)
  pure (xs !! i)

pickIO
  :: [Random a]
  -> Random a
pickIO ios = do
  io <- pickFrom ios
  io

pickName
  :: Random String
pickName = Random $ do
  name : names <- get
  put names
  pure name

pickListGiven
  :: KnownSizeGiven a
  => Int  -- desired length
  -> (Int -> Random a)
  -> (Int -> Random [a])
pickListGiven 0 _ _ = do
  pure []
pickListGiven desiredLength pickA m = do
  a <- pickA m
  as <- pickListGiven (desiredLength - 1) pickA (knownSizeGiven a m)
  pure (a:as)
