{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, PolyKinds, RankNTypes, ScopedTypeVariables, TypeApplications, TypeOperators #-}
{-# OPTIONS -Wno-name-shadowing #-}
module UntypedPremonoidal where

import Control.Monad (replicateM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, get, put)
import Data.Constraint (Dict(..), withDict)
import Data.Kind (Type)
import Data.List (intercalate)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.TypeLits
import System.Random (randomRIO)
import Unsafe.Coerce (unsafeCoerce)


intVal
  :: KnownNat n
  => Proxy n
  -> Int
intVal
  = fromIntegral . natVal

someIntVal
  :: Int
  -> Maybe SomeNat
someIntVal
  = someNatVal . fromIntegral


data Const2 a m n = Const2
  { runConst2 :: a }

data Free (q :: Nat -> Nat -> Type)
          (m :: Nat)
          (n :: Nat)
          where
  Id
    :: KnownNat n
    => Free q n n
  (:>>>)
    :: q m n
    -> Free q n o
    -> Free q m o
infixr 4 :>>>

data Either2 (q :: Nat -> Nat -> Type)
             (r :: Nat -> Nat -> Type)
             (m :: Nat)
             (n :: Nat)
             where
  L2
    :: q m n
    -> Either2 q r m n
  R2
    :: r m n
    -> Either2 q r m n

data Widen (q :: Nat -> Nat -> Type)
           (m :: Nat)
           (n :: Nat)
           where
  Widen
    :: (KnownNat pre, KnownNat post)
    => Proxy pre
    -> q m' n'
    -> Proxy post
    -> Widen q (pre + m' + post)
               (pre + n' + post)

data Atom (q :: Nat -> Nat -> Type)
          (m :: Nat)
          (n :: Nat)
          where
  Atom
    :: (KnownNat m, KnownNat n)
    => q m n
    -> Atom q m n

data Intro (m :: Nat)
           (n :: Nat)
           where
  Intro
    :: Intro 0 1

data Swap (m :: Nat)
          (n :: Nat)
          where
  Swap
    :: Swap 2 2

data Drop (m :: Nat)
          (n :: Nat)
          where
  Drop
    :: Drop 1 0

data Dup (m :: Nat)
         (n :: Nat)
         where
  Dup
    :: Dup 1 2

type PremonoidalStep
  = Intro

type LinearStep
  = Intro `Either2` Swap

type AffineStep
  = Intro `Either2` Swap `Either2` Drop

type CartesianStep
  = Intro `Either2` Swap `Either2` Drop `Either2` Dup

type StringDiagram step q
  = Free (Widen (step `Either2` Atom q))

type Premonoidal q
  = StringDiagram PremonoidalStep q

type Linear q
  = StringDiagram LinearStep q

type Affine q
  = StringDiagram AffineStep q

type Cartesian q
  = StringDiagram CartesianStep q

data Some (q :: Nat -> Type) where
  Some
    :: KnownNat n
    => Proxy n
    -> q n
    -> Some q

data Some2 (q :: Nat -> Nat -> Type) where
  Some2
    :: (KnownNat m, KnownNat n)
    => Proxy m
    -> Proxy n
    -> q m n
    -> Some2 q


hoistFree
  :: (forall x y. q x y -> r x y)
  -> Free q m n
  -> Free r m n
hoistFree f = \case
  Id
    -> Id
  q :>>> qs
    -> f q :>>> hoistFree f qs

hoistWiden
  :: (forall x y. q x y -> r x y)
  -> Widen q m n
  -> Widen r m n
hoistWiden f (Widen proxyPre q proxyPost)
  = Widen proxyPre (f q) proxyPost

hoistSome
  :: (forall x. q x -> r x)
  -> Some q
  -> Some r
hoistSome f (Some proxyM q)
  = Some proxyM (f q)

hoistSome2
  :: (forall x y. q x y -> r x y)
  -> Some2 q
  -> Some2 r
hoistSome2 f (Some2 proxyM proxyN q)
  = Some2 proxyM proxyN(f q)


axiom :: forall a b. Dict (a ~ b)
axiom = unsafeCoerce (Dict :: Dict (a ~ a))

withKnownSum
  :: forall m n r. (KnownNat m, KnownNat n)
  => Proxy m
  -> Proxy n
  -> (KnownNat (m + n) => Proxy (m + n) -> r)
  -> r
withKnownSum proxyM proxyN cc
  = case someIntVal (intVal proxyM + intVal proxyN) of
      Just (SomeNat proxySum)
        -> go proxySum
      Nothing
        -> error $ "impossible: the sum of two Nats"
                ++ show (intVal proxyM)
                ++ " + "
                ++ show (intVal proxyN)
                ++ " is somehow negative??"
  where
    go
      :: forall sum. KnownNat sum
      => Proxy sum
      -> r
    go proxySum
      = withDict (axiom :: Dict (sum ~ (m + n)))
      $ cc proxySum

class KnownSize q where
  withKnownSize
    :: q m n
    -> ( (KnownNat m, KnownNat n)
      => Proxy m -> Proxy n -> r
       )
    -> r

instance KnownSize q => KnownSize (Free q) where
  withKnownSize Id cc
    = cc Proxy Proxy
  withKnownSize (q01 :>>> qs1Z) cc
    = withKnownSize q01 $ \proxyM _
   -> withKnownSize qs1Z $ \_ proxyN
   -> cc proxyM proxyN

instance (KnownSize q, KnownSize r) => KnownSize (Either2 q r) where
  withKnownSize (L2 q) cc
    = withKnownSize q cc
  withKnownSize (R2 q) cc
    = withKnownSize q cc

instance KnownSize q => KnownSize (Widen q) where
  withKnownSize (Widen proxyPre q proxyPost) cc
    = withKnownSize q $ \proxyM' proxyN'
   -> withKnownSum proxyPre proxyM' $ \proxyPreM'
   -> withKnownSum proxyPre proxyN' $ \proxyPreN'
   -> withKnownSum proxyPreM' proxyPost $ \proxyM
   -> withKnownSum proxyPreN' proxyPost $ \proxyN
   -> cc proxyM proxyN

instance KnownSize (Atom q) where
  withKnownSize (Atom _) cc
    = cc Proxy Proxy

instance KnownSize Intro where
  withKnownSize Intro cc
    = cc Proxy Proxy

instance KnownSize Swap where
  withKnownSize Swap cc
    = cc Proxy Proxy

instance KnownSize Drop where
  withKnownSize Drop cc
    = cc Proxy Proxy

instance KnownSize Dup where
  withKnownSize Dup cc
    = cc Proxy Proxy


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

pickSomeNat
  :: Int
  -> Random SomeNat
pickSomeNat maxNat = do
  n <- pickFrom [0..maxNat]
  case someIntVal n of
    Just someNat -> do
      pure someNat
    Nothing -> do
      error $ "impossible: 0.."
           ++ show maxNat
           ++ " somehow contains a negative number??"

pickConst2
  :: Random (Const2 String m n)
pickConst2 = Random $ do
  name : names <- get
  put names
  pure $ Const2 name

pickSomeFree
  :: KnownNat m
  => Int
  -> ( forall x. KnownNat x
    => Random (Some (q x))
     )
  -> Random (Some (Free q m))
pickSomeFree 0 _ = do
  pure $ Some Proxy Id
pickSomeFree size pickSomeQ = do
  Some _proxyN q <- pickSomeQ
  Some proxyO qs <- pickSomeFree (size - 1) pickSomeQ
  pure $ Some proxyO (q :>>> qs)

pickSomeWiden
  :: forall m q. KnownNat m
  => Some2 q
  -> Random (Some (Widen q m))
pickSomeWiden (Some2 proxyM' proxyN' q) = do
  go proxyM' proxyN' q
  where
    proxyM :: Proxy m
    proxyM = Proxy

    go
      :: forall m' n'. (KnownNat m', KnownNat n')
      => Proxy m'
      -> Proxy n'
      -> q m' n'
      -> Random (Some (Widen q m))
    go proxyM' proxyN' q = do
      let extraM = m - m'
      pre <- pickFrom [0..extraM]
      let post = extraM - pre
      case (someIntVal pre, someIntVal post) of
        (Just (SomeNat proxyPre), Just (SomeNat proxyPost)) -> do
          withKnownSum proxyPre proxyM' $ \proxyPreM' -> do
            withKnownSum proxyPre proxyN' $ \proxyPreN' -> do
              withKnownSum proxyPreM' proxyPost $ \_ -> do
                withKnownSum proxyPreN' proxyPost $ \_ -> do
                  go' proxyPre proxyPost
        _ -> do
          error $ "pickSomeWidening: "
               ++ show m'
               ++ " > "
               ++ show m
      where
        m = intVal proxyM
        m' = intVal proxyM'

        go'
          :: forall pre post
           . ( KnownNat pre
             , KnownNat post
             , KnownNat (pre + m' + post)
             , KnownNat (pre + n' + post)
             )
          => Proxy pre
          -> Proxy post
          -> Random (Some (Widen q m))
        go' proxyPre proxyPost = do
          case sameNat (Proxy @m) (Proxy @(pre + m' + post)) of
            Just Refl -> do
              pure $ Some Proxy $ Widen proxyPre q proxyPost
            Nothing -> do
              error $ "impossible: "
                   ++ show m
                   ++ " is somehow not equal to "
                   ++ show (pre + m' + ((m - m') - pre))
                   ++ "??"
          where
            pre = intVal proxyPre

class FanOut (q :: Nat -> Nat -> Type) where
  fanOut
    :: KnownNat m
    => Proxy m
    -> [Random (Some (Widen q m))]

pickSomeFanOut
  :: forall q m. (FanOut q, KnownNat m)
  => Random (Some (Widen q m))
pickSomeFanOut = do
  pickIO (fanOut Proxy)

instance (FanOut q, FanOut r) => FanOut (Either2 q r) where
  fanOut proxyM
    = fmap (fmap (hoistSome (hoistWiden L2))) (fanOut proxyM)
   ++ fmap (fmap (hoistSome (hoistWiden R2))) (fanOut proxyM)

instance FanOut (Atom (Const2 String)) where
  fanOut proxyM
    = [ do let m = intVal proxyM
           m' <- pickFrom [0..(m `min` 3)]
           n' <- pickFrom [0..3]
           case (someIntVal m', someIntVal n') of
             (Just (SomeNat proxyM'), Just (SomeNat proxyN')) -> do
               c2 <- pickConst2
               pickSomeWiden $ Some2 proxyM' proxyN' $ Atom c2
             _ -> do
               error $ "impossible: one of "
                    ++ show m'
                    ++ " or "
                    ++ show n'
                    ++ " is negative even though they were drawn from [0..3]??"
      ]

instance FanOut Intro where
  fanOut _
    = [ pickSomeWiden $ Some2 Proxy Proxy Intro
      ]

instance FanOut Swap where
  fanOut proxyM
    = [ pickSomeWiden $ Some2 Proxy Proxy Swap
      | intVal proxyM >= 2
      ]

instance FanOut Drop where
  fanOut proxyM
    = [ pickSomeWiden $ Some2 Proxy Proxy Drop
      | intVal proxyM >= 1
      ]

instance FanOut Dup where
  fanOut proxyM
    = [ pickSomeWiden $ Some2 Proxy Proxy Dup
      | intVal proxyM >= 1
      ]

pickSomeStringDiagram
  :: forall step m. (FanOut step, KnownNat m)
  => Int
  -> Random (Some (StringDiagram step (Const2 String) m))
pickSomeStringDiagram size = do
  pickSomeFree size pickSomeFanOut

pickSome2StringDiagram
  :: forall step. FanOut step
  => Int
  -> Random (Some2 (StringDiagram step (Const2 String)))
pickSome2StringDiagram size = do
  SomeNat proxyM <- pickSomeNat 5
  Some proxyN qs <- pickSomeStringDiagram size
  pure $ Some2 proxyM proxyN qs


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

class KnownSize q => PPrint q where
  pprint
    :: KnownNat m
    => Proxy m
    -> Proxy n
    -> q m n
    -> [String]

pprintSome2
  :: PPrint q
  => Some2 q
  -> [String]
pprintSome2 (Some2 proxyM proxyN q)
  = pprint proxyM proxyN q

instance PPrint q => PPrint (Free q) where
  pprint proxyM proxyO = \case
    Id
      -> [pprint1Pipes m]
    step :>>> qs
      -> withKnownSize step $ \_ proxyN
      -> [pprint1Pipes m]
      ++ pprint proxyM proxyN step
      ++ pprint proxyN proxyO qs
    where
      m = intVal proxyM

instance (PPrint q, PPrint r) => PPrint (Either2 q r) where
  pprint proxyM proxyN = \case
    L2 q
      -> pprint proxyM proxyN q
    R2 r
      -> pprint proxyM proxyN r

----   [ | | ]
---- > [+---+]
---- > [| f |]
---- > [+---+]
----   [ |   ]
instance PPrint (Atom (Const2 String)) where
  pprint proxyM proxyN (Atom (Const2 label))
    = let m = intVal proxyM
          n = intVal proxyN
          w = 1 `max` m `max` n
   in [pprint1Dashes w]
   ++ [pprint1Label w label]
   ++ [pprint1Dashes w]

--   [ | | ]
-- > [  \ \]
-- > [+-+| | ]
-- > [|f|| | ]
-- > [+-+| | ]
-- > [  / /  ]
--   [ | | ]
instance PPrint q => PPrint (Widen q) where
  pprint _ _ (Widen proxyPre q proxyPost)
    = let pre = intVal proxyPre
          post = intVal proxyPost
   in withKnownSize q $ \proxyM' proxyN'
   -> let m' = intVal proxyM'
          n' = intVal proxyN'
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
      | s <- pprint proxyM' proxyN' q
      ]
   ++ [ pprint1Pipes (pre + n')
    .+. pprint1Spaces blanks
    .+. pprint1Slashes post
      | let gap = w' - n'
      , blanks <- [gap,gap-1..1]
      , post > 0
      ]

--   [   ]
-- > [ . ]
--   [ | ]
instance PPrint Intro where
  pprint _ _ Intro
    = [" . "]

--   [ | | ]
-- > [  X  ]
--   [ | | ]
instance PPrint Swap where
  pprint _ _ Swap
    = ["  X  "]

--   [ | ]
-- > [ x ]
--   [   ]
instance PPrint Drop where
  pprint _ _ Drop
    = [" x "]

--   [ | ]
-- > [ |\  ]
--   [ | | ]
instance PPrint Dup where
  pprint _ _ Dup
    = [" |\\  "]


test :: IO ()
test = do
  some2qs <- runRandom $ pickSome2StringDiagram @CartesianStep 6
  mapM_ putStrLn $ pprintSome2 some2qs
