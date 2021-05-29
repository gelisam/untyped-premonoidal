{-# LANGUAGE DefaultSignatures, DeriveFunctor, FlexibleInstances, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module UntypedPremonoidal where

import Control.Monad (replicateM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, get, put)
import Data.List (intercalate)
import Data.Void (Void, absurd)
import System.Random (randomRIO)


data Widen a = Widen
  { widenPre :: Int
  , widenMid :: a
  , widenPost :: Int
  }
  deriving Functor

data Atom a = Atom
  { atomInput :: Int
  , atomValue :: a
  , atomOutput :: Int
  }
  deriving Functor

data Swap = Swap

data Drop = Drop

data Dup = Dup

type PremonoidalStep
  = Void

type LinearStep
  = Swap

type AffineStep
  = Swap `Either` Drop

type CartesianStep
  = Swap `Either` Drop `Either` Dup

type StringDiagram step q
  = [Widen (step `Either` Atom q)]

type Premonoidal q
  = StringDiagram PremonoidalStep q

type Linear q
  = StringDiagram LinearStep q

type Affine q
  = StringDiagram AffineStep q

type Cartesian q
  = StringDiagram CartesianStep q


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

instance KnownSize a => KnownSize (Widen a) where
  knownSize (Widen pre a post)
    = let (m', n') = knownSize a
   in ( pre + m' + post
      , pre + n' + post
      )

instance KnownSize (Atom a) where
  knownSize (Atom m _ n)
    = (m, n)

instance KnownSize Swap where
  knownSize Swap
    = (2, 2)

instance KnownSize Drop where
  knownSize Drop
    = (1, 0)

instance KnownSize Dup where
  knownSize Dup
    = (1, 2)


-- Sometimes we can't measure the input size, but we can compute the output
-- size from the input size. Throughout this file, functions which need to be
-- given the input size are suffixed with "Given".
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

instance KnownSizeGiven Void
instance KnownSizeGiven (Atom a)
instance KnownSizeGiven Swap
instance KnownSizeGiven Drop
instance KnownSizeGiven Dup

instance KnownSizeGiven a => KnownSizeGiven [a] where
  knownSizeGiven []
    = id
  knownSizeGiven (a:as)
    = knownSizeGiven as . knownSizeGiven a

instance (KnownSizeGiven a, KnownSizeGiven b) => KnownSizeGiven (Either a b) where
  knownSizeGiven (Left a)
    = knownSizeGiven a
  knownSizeGiven (Right b)
    = knownSizeGiven b

instance KnownSizeGiven a => KnownSizeGiven (Widen a) where
  knownSizeGiven (Widen pre a post) m
    = knownSizeGiven a (m - pre - post) + pre + post


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

pickWidenGiven
  :: KnownSize a
  => a
  -> (Int -> Random (Widen a))
pickWidenGiven a m = do
  let (m', _n') = knownSize a
  let extraM = m - m'
  pre <- pickFrom [0..extraM]
  let post = extraM - pre
  pure $ Widen pre a post

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

instance PickGiven (Atom String) where
  pickingsGiven m
    = [ do m' <- pickFrom [0..(m `min` 3)]
           n' <- pickFrom [0..3]
           s <- pickName
           pickWidenGiven (Atom m' s n') m
      ]

instance PickGiven Swap where
  pickingsGiven m
    = [ pickWidenGiven Swap m
      | m >= 2
      ]

instance PickGiven Drop where
  pickingsGiven m
    = [ pickWidenGiven Drop m
      | m >= 1
      ]

instance PickGiven Dup where
  pickingsGiven m
    = [ pickWidenGiven Dup m
      | m >= 1
      ]

pickStringDiagramGiven
  :: forall step
   . (PickGiven step, KnownSizeGiven step)
  => Int  -- desired length
  -> (Int -> Random (StringDiagram step String))
pickStringDiagramGiven desiredLength
  = pickListGiven desiredLength pickGiven


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

--   [ | | | ]
-- > [/ / /  ]
--   [ | | ]
pprint1Slashes
  :: Int -> String
pprint1Slashes n
  = concat (replicate n "/ ")
 ++ " "

--   [ | | | ]
-- > [  \ \ \]
--     [ | | ]
pprint1Backslashes
  :: Int -> String
pprint1Backslashes n
  = " "
 ++ concat (replicate n " \\")

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

--   [ | | ]
-- > [  \ \]
-- > [+-+| | ]
-- > [|f|| | ]
-- > [+-+| | ]
-- > [  / /  ]
--   [ | | ]
pprintWiden
  :: Int  -- input size
  -> Widen [String]
  -> Int  -- output size
  -> [String]
pprintWiden m (Widen pre pprintedA post) n
    = let m' = m - pre - post
          n' = n - pre - post
          w' = 1 `max` m' `max` n'
   in [ pprint1Pipes (pre + m')
    .+. pprint1Spaces blanks
    .+. pprint1Backslashes post
      | let gap = w' - m'
      , blanks <- [0..gap-1]
      , post > 0
      ]
   ++ [ pprint1Pipes pre
    .+. s
    .+. pprint1Pipes post
      | s <- pprintedA
      ]
   ++ [ pprint1Pipes (pre + n')
    .+. pprint1Spaces blanks
    .+. pprint1Slashes post
      | let gap = w' - n'
      , blanks <- [gap,gap-1..1]
      , post > 0
      ]


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

instance PPrint a => PPrint (Widen a) where
  pprint (Widen pre a post)
    = let (m', n') = knownSize a
   in pprintWiden
        (pre + m' + post)
        (Widen pre (pprint a) post)
        (pre + n' + post)

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

--   [ | | ]
-- > [  X  ]
--   [ | | ]
instance PPrint Swap where
  pprint Swap
    = ["  X  "]

--   [ | ]
-- > [ x ]
--   [   ]
instance PPrint Drop where
  pprint Drop
    = [" x "]

--   [ | ]
-- > [ |\  ]
--   [ | | ]
instance PPrint Dup where
  pprint Dup
    = [" |\\  "]


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

instance PPrintGiven Void
instance PPrintGiven (Atom String)
instance PPrintGiven Swap
instance PPrintGiven Drop
instance PPrintGiven Dup

instance PPrintGiven a => PPrintGiven [a] where
  pprintGiven [] m
    = [pprint1Pipes m]
  pprintGiven (a:as) m
    = let n = knownSizeGiven a m
   in [pprint1Pipes m]
   ++ pprintGiven a m
   ++ pprintGiven as n

instance (PPrintGiven a, PPrintGiven b) => PPrintGiven (Either a b) where
  pprintGiven (Left a)
    = pprintGiven a
  pprintGiven (Right b)
    = pprintGiven b

instance PPrintGiven a => PPrintGiven (Widen a) where
  pprintGiven (Widen pre a post) m
    = let m' = m - pre - post
          n' = knownSizeGiven a m'
          n = pre + n' + post
   in pprintWiden
        m
        (Widen pre (pprintGiven a m') post)
        n


test :: IO ()
test = do
  (m, as) <- runRandom $ do
    m <- pickFrom [0..5]
    as <- pickStringDiagramGiven @CartesianStep 6 m
    pure (m, as)
  mapM_ putStrLn $ pprintGiven as m
