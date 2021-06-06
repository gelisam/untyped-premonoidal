module UntypedPremonoidal.WidenPickings where

import Data.Void (Void)

import UntypedPremonoidal.Random
import UntypedPremonoidal.Widen


class WidenPickings a where
  widenPickingsGiven
    :: Int -> [Random (Widen a)]

pickGiven
  :: WidenPickings a
  => Int -> Random (Widen a)
pickGiven m = do
  pickIO (widenPickingsGiven m)

instance WidenPickings Void where
  widenPickingsGiven _
    = []

instance (WidenPickings a, WidenPickings b) => WidenPickings (Either a b) where
  widenPickingsGiven m
    = (fmap . fmap . fmap) Left (widenPickingsGiven m)
   ++ (fmap . fmap . fmap) Right (widenPickingsGiven m)
