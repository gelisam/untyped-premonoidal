{-# LANGUAGE DefaultSignatures #-}
module UntypedPremonoidal.KnownSize where

import Data.Void (Void, absurd)


class KnownSize a where
  knownSize
    :: a
    -> ( Int  -- input size
       , Int  -- output size
       )

instance KnownSize Void where
  knownSize
    = absurd

instance (KnownSize a, KnownSize b) => KnownSize (Either a b) where
  knownSize (Left a)
    = knownSize a
  knownSize (Right b)
    = knownSize b


-- Sometimes we can't measure the input size, but we can compute the output
-- size from the input size. Throughout this project, functions which need to
-- be given the input size are suffixed with "Given".
class KnownSizeGiven a where
  knownSizeGiven
    :: a
    -> Int  -- input size
    -> Int  -- output size
  default knownSizeGiven
    :: KnownSize a
    => a
    -> (Int -> Int)
  knownSizeGiven a m
    = let (expectedM, n) = knownSize a
   in if m == expectedM
      then n
      else error $ "knownSizeGiven: input is known to be "
                ++ show expectedM
                ++ ", but the given size was "
                ++ show m

instance KnownSizeGiven a => KnownSizeGiven [a] where
  knownSizeGiven []
    = id
  knownSizeGiven (a:as)
    = knownSizeGiven as . knownSizeGiven a

instance KnownSizeGiven Void

instance (KnownSizeGiven a, KnownSizeGiven b) => KnownSizeGiven (Either a b) where
  knownSizeGiven (Left a)
    = knownSizeGiven a
  knownSizeGiven (Right b)
    = knownSizeGiven b
