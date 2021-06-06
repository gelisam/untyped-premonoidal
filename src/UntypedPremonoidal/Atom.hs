{-# LANGUAGE DeriveFunctor, FlexibleInstances, TypeApplications #-}
module UntypedPremonoidal.Atom where

import Control.Monad (replicateM)
import Data.Dynamic (Dynamic)
import Data.List (intercalate)
import Data.Traversable (forM)
import qualified Data.Map as Map

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

instance (PickAtom a, PickAtom b) => PickAtom (a, b) where
  pickAtom m n
      = (,)
   <$> pickAtom m n
   <*> pickAtom m n

instance PickAtom String where
  pickAtom _ _ = do
    pickName

instance PickAtom ([Bool] -> [Bool]) where
  pickAtom m n = do
    let inputs = replicateM @[] m [False,True]
    pairs <- forM inputs $ \input -> do
      output <- replicateM @Random n $ do
        pickFrom [False,True]
      pure (input, output)
    let table = Map.fromList pairs
    pure (table Map.!)


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
