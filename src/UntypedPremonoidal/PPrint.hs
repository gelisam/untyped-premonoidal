{-# LANGUAGE DefaultSignatures #-}
module UntypedPremonoidal.PPrint where

import Data.Void (Void, absurd)

import UntypedPremonoidal.KnownSize


-- convention: when drawing something N pipes wide, every line should have
-- 2N+1 characters, e.g. " | | | "

-- The two edges of those 2N+1 characters are usually spaces. This
-- combinator concatenates two such segments, overwriting one of the
-- spaces (throwing an exception if that's not possible) so that the
-- result is still 2M+1 characters.
(.+.)
  :: String -> String -> String
(.+.) xs ys
  | last xs == ' '
    = init xs ++ ys
  | head ys == ' '
    = xs ++ tail ys
  | otherwise
    = error $ "(.+.): neither the end of "
           ++ show xs
           ++ " nor the beginning of "
           ++ show ys
           ++ " is a space"

-- > [ | | | ]
pprint1Pipes
  :: Int -> String
pprint1Pipes n
  = " "
 ++ concat (replicate n "| ")

--   [ | | | ]
-- > [       ]
--   [ | | | ]
pprint1Spaces
  :: Int -> String
pprint1Spaces n
  = replicate (2 * n + 1) ' '


class KnownSize a => PPrint a where
  pprint
    :: a
    -> [String]

instance PPrint Void where
  pprint
    = absurd

instance (PPrint a, PPrint b) => PPrint (Either a b) where
  pprint (Left a)
    = pprint a
  pprint (Right b)
    = pprint b


class KnownSizeGiven a => PPrintGiven a where
  pprintGiven
    :: a
    -> (Int -> [String])
  default pprintGiven
    :: PPrint a
    => a
    -> (Int -> [String])
  pprintGiven a m
    = let (expectedM, _) = knownSize a
   in if m == expectedM
      then pprint a
      else error $ "pprintGiven: input is known to be "
                ++ show expectedM
                ++ ", but the given size was "
                ++ show m

instance PPrintGiven a => PPrintGiven [a] where
  pprintGiven [] m
    = [pprint1Pipes m]
  pprintGiven (a:as) m
    = let n = knownSizeGiven a m
   in [pprint1Pipes m]
   ++ pprintGiven a m
   ++ pprintGiven as n

instance PPrintGiven Void

instance (PPrintGiven a, PPrintGiven b) => PPrintGiven (Either a b) where
  pprintGiven (Left a)
    = pprintGiven a
  pprintGiven (Right b)
    = pprintGiven b
