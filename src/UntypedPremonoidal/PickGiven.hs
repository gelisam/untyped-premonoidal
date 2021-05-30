module UntypedPremonoidal.PickGiven where

import Data.Void (Void)

import UntypedPremonoidal.Random
import UntypedPremonoidal.Widen


class PickGiven a where
  pickingsGiven
    :: Int -> [Random (Widen a)]

pickGiven
  :: PickGiven a
  => Int -> Random (Widen a)
pickGiven m = do
  pickIO (pickingsGiven m)

instance PickGiven Void where
  pickingsGiven _
    = []

instance (PickGiven a, PickGiven b) => PickGiven (Either a b) where
  pickingsGiven m
    = (fmap . fmap . fmap) Left (pickingsGiven m)
   ++ (fmap . fmap . fmap) Right (pickingsGiven m)
