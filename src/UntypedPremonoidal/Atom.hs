{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}
module UntypedPremonoidal.Atom where

import Data.Dynamic (Dynamic)
import Data.List (intercalate)

import UntypedPremonoidal.Interpret
import UntypedPremonoidal.KnownSize
import UntypedPremonoidal.PPrint
import UntypedPremonoidal.Random
import UntypedPremonoidal.Widen
import UntypedPremonoidal.WidenPickings


data Atom a = Atom
  { atomInput :: Int
  , atomValue :: a
  , atomOutput :: Int
  }
  deriving Functor


instance Interpret (Atom ([Dynamic] -> [Dynamic])) where
  interpret (Atom input f output) xs
    | length xs /= input
      = error $ "interpret Atom: input should have length "
             ++ show input
             ++ "but the given list has length "
             ++ show (length xs)
    | length ys /= output
      = error $ "interpret Atom: output should have length "
             ++ show output
             ++ "but the function returned a list of length "
             ++ show (length ys)
    | otherwise
      = ys
    where
      ys = f xs


instance KnownSize (Atom a) where
  knownSize (Atom m _ n)
    = (m, n)

instance KnownSizeGiven (Atom a)


class PickAtom a where
  pickAtom
    :: Int -> Int -> Random a

instance PickAtom String where
  pickAtom _ _
    = pickName

instance PickAtom a => WidenPickings (Atom a) where
  widenPickingsGiven m
    = [ do m' <- pickFrom [0..(m `min` 3)]
           n' <- pickFrom [0..3]
           s <- pickAtom m' n'
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
