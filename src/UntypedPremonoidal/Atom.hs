{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}
module UntypedPremonoidal.Atom where

import Data.List (intercalate)

import UntypedPremonoidal.KnownSize
import UntypedPremonoidal.PPrint
import UntypedPremonoidal.PickGiven
import UntypedPremonoidal.Random
import UntypedPremonoidal.Widen


data Atom a = Atom
  { atomInput :: Int
  , atomValue :: a
  , atomOutput :: Int
  }
  deriving Functor


instance KnownSize (Atom a) where
  knownSize (Atom m _ n)
    = (m, n)

instance KnownSizeGiven (Atom a)


instance PickGiven (Atom String) where
  pickingsGiven m
    = [ do m' <- pickFrom [0..(m `min` 3)]
           n' <- pickFrom [0..3]
           s <- pickName
           pickWidenGiven (Atom m' s n') m
      ]


--   [ | | | ]
-- > [+-----+]
--   [ | | | ]
pprint1Dashes
  :: Int -> String
pprint1Dashes n
  = "+"
 ++ intercalate "-" (replicate n "-")
 ++ "+"

--   [+-----+]
-- > [|  f  |]
--   [+-----+]
pprint1Label
  :: Int -> String -> String
pprint1Label n label
  = "|"
 ++ replicate leftHalf ' '
 ++ label
 ++ replicate rightHalf ' '
 ++ "|"
  where
    w = 2 * n - 1 - length label
    leftHalf = w `div` 2
    rightHalf = w - leftHalf

----   [ | | ]
---- > [+---+]
---- > [| f |]
---- > [+---+]
----   [ |   ]
instance PPrint (Atom String) where
  pprint (Atom m label n)
    = let w = 1 `max` m `max` n
   in [pprint1Dashes w]
   ++ [pprint1Label w label]
   ++ [pprint1Dashes w]

instance PPrintGiven (Atom String)
