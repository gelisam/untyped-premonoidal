{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, KindSignatures, LambdaCase, PolyKinds, RankNTypes, ScopedTypeVariables, TypeApplications, TypeOperators #-}
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

data FreeCategory (q :: Nat -> Nat -> Type)
                  (m :: Nat)
                  (n :: Nat)
                  where
  Id
    :: KnownNat n
    => FreeCategory q n n
  (:>>>)
    :: q m n
    -> FreeCategory q n o
    -> FreeCategory q m o
infixr 4 :>>>

data Step (q :: Nat -> Nat -> Type)
          (m :: Nat)
          (n :: Nat)
          where
  Atom
    :: (KnownNat m, KnownNat n)
    => q m n
    -> Step q m n
  Widen
    :: (KnownNat pre, KnownNat post)
    => Proxy pre
    -> Step q m n
    -> Proxy post
    -> Step q (pre + m + post)
              (pre + n + post)
  Swap
    :: Step q 2 2
  Intro
    :: Step q 0 1
  Drop
    :: Step q 1 0
  Dup
    :: Step q 1 2

type StringDiagram q
  = FreeCategory (Step q)

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

withKnownFreeCategorySize
  :: forall q m n r
   . ( forall x y
     . q x y
    -> ( (KnownNat x, KnownNat y)
      => Proxy x -> Proxy y -> r
       )
    -> r
     )
  -> FreeCategory q m n
  -> ( (KnownNat m, KnownNat n)
    => Proxy m -> Proxy n -> r
     )
  -> r
withKnownFreeCategorySize withKnownQSize qs0Z cc
  = case qs0Z of
      Id
        -> cc Proxy Proxy
      q01 :>>> qs1Z
        -> withKnownQSize q01 $ \proxyM _
        -> withKnownFreeCategorySize withKnownQSize qs1Z $ \_ proxyN
        -> cc proxyM proxyN

withKnownStepSize
  :: forall q m n r
   . Step q m n
  -> ( (KnownNat m, KnownNat n)
    => Proxy m -> Proxy n -> r
     )
  -> r
withKnownStepSize step cc
  = case step of
      Atom _
        -> cc Proxy Proxy
      Widen proxyPre subStep proxyPost
        -> withKnownStepSize subStep $ \proxyM' proxyN'
        -> withKnownSum proxyPre proxyM' $ \proxyPreM'
        -> withKnownSum proxyPre proxyN' $ \proxyPreN'
        -> withKnownSum proxyPreM' proxyPost $ \proxyM
        -> withKnownSum proxyPreN' proxyPost $ \proxyN
        -> cc proxyM proxyN
      Swap
        -> cc Proxy Proxy
      Intro
        -> cc Proxy Proxy
      Drop
        -> cc Proxy Proxy
      Dup
        -> cc Proxy Proxy

withKnownStringDiagramSize
  :: forall q m n r
   . StringDiagram q m n
  -> ( (KnownNat m, KnownNat n)
    => Proxy m -> Proxy n -> r
     )
  -> r
withKnownStringDiagramSize
  = withKnownFreeCategorySize withKnownStepSize


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

pickSomeConst2
  :: Random (Some (Const2 String m))
pickSomeConst2 = do
  SomeNat proxyN <- pickSomeNat 3
  c2 <- pickConst2
  pure $ Some proxyN c2

pickSome2Const2
  :: Random (Some2 (Const2 String))
pickSome2Const2 = do
  SomeNat proxyM <- pickSomeNat 3
  Some proxyN c2 <- pickSomeConst2
  pure $ Some2 proxyM proxyN c2

pickSomeFreeCategory
  :: KnownNat m
  => Int
  -> ( forall x. KnownNat x
    => Random (Some (q x))
     )
  -> Random (Some (FreeCategory q m))
pickSomeFreeCategory 0 _ = do
  pure $ Some Proxy Id
pickSomeFreeCategory size pickSomeQ = do
  Some _proxyN q <- pickSomeQ
  Some proxyO qs <- pickSomeFreeCategory (size - 1) pickSomeQ
  pure $ Some proxyO (q :>>> qs)

pickSomeWidening
  :: forall m q. KnownNat m
  => Some2 (Step q)
  -> Random (Some (Step q m))
pickSomeWidening (Some2 proxyM' proxyN' step) = do
  go proxyM' proxyN' step
  where
    proxyM :: Proxy m
    proxyM = Proxy

    go
      :: forall m' n'. (KnownNat m', KnownNat n')
      => Proxy m'
      -> Proxy n'
      -> Step q m' n'
      -> Random (Some (Step q m))
    go proxyM' proxyN' step = do
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
          -> Random (Some (Step q m))
        go' proxyPre proxyPost = do
          case sameNat (Proxy @m) (Proxy @(pre + m' + post)) of
            Just Refl -> do
              pure $ Some Proxy $ Widen proxyPre step proxyPost
            Nothing -> do
              error $ "impossible: "
                   ++ show m
                   ++ " is somehow not equal to "
                   ++ show (pre + m' + ((m - m') - pre))
                   ++ "??"
          where
            pre = intVal proxyPre

pickSomeStep
  :: forall m. KnownNat m
  => Random (Some (Step (Const2 String) m))
pickSomeStep = do
  case sameNat (Proxy @m) (Proxy @0) of
    Just Refl -> do
      pickIO
        [ pure $ Some (Proxy @1) Intro
        , do Some proxyN c2 <- pickSomeConst2
             pure $ Some proxyN $ Atom c2
        ]
    Nothing -> do
      case sameNat (Proxy @m) (Proxy @1) of
        Just Refl -> do
          pickIO
            [ pure $ Some (Proxy @0) Drop
            , pure $ Some (Proxy @2) Dup
            , do Some proxyN c2 <- pickSomeConst2
                 pure $ Some proxyN $ Atom c2
            ]
        Nothing -> do
          case sameNat (Proxy @m) (Proxy @2) of
            Just Refl -> do
              pickIO
                [ pure $ Some (Proxy @2) Swap
                , do Some proxyN c2 <- pickSomeConst2
                     pure $ Some proxyN $ Atom c2
                ]
            Nothing -> do
              some2Step <- pickSome2Step
              pickSomeWidening some2Step

pickSome2Step
  :: Random (Some2 (Step (Const2 String)))
pickSome2Step = do
  pickIO
    [ do Some2 proxyM proxyN c2 <- pickSome2Const2
         pure $ Some2 proxyM proxyN $ Atom c2
    , pure $ Some2 (Proxy @2) (Proxy @2) Swap
    , pure $ Some2 (Proxy @0) (Proxy @1) Intro
    , pure $ Some2 (Proxy @1) (Proxy @0) Drop
    , pure $ Some2 (Proxy @1) (Proxy @2) Dup
    ]

pickSomeStringDiagram
  :: KnownNat m
  => Int
  -> Random (Some (StringDiagram (Const2 String) m))
pickSomeStringDiagram size = do
  pickSomeFreeCategory size pickSomeStep

pickSome2StringDiagram
  :: Int
  -> Random (Some2 (StringDiagram (Const2 String)))
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

--   [ | ]
-- > [ |\  ]
--   [ | | ]
pprintStep
  :: forall m n. KnownNat m
  => Step (Const2 String) m n
  -> [String]
pprintStep = \case
  Atom (Const2 label)
    -> let n = intVal (Proxy @n)
           w = 1 `max` m `max` n
    in [pprint1Dashes w]
    ++ [pprint1Label w label]
    ++ [pprint1Dashes w]
  Widen proxyPre step proxyPost
    -> let pre = intVal proxyPre
           post = intVal proxyPost
    in withKnownStepSize step $ \proxyM' proxyN'
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
       | s <- pprintStep step
       ]
    ++ [ pprint1Pipes (pre + n')
     .+. pprint1Spaces blanks
     .+. pprint1Slashes post
       | let gap = w' - n'
       , blanks <- [gap,gap-1..1]
       , post > 0
       ]
  Swap
    --  " | | "
    -> ["  X  "]
    --  " | | "
  Intro
    -> [" . "]
    --  " | "
  Drop
    --  " | "
    -> [" x "]
  Dup
    --  " | "
    -> [" |\\  "]
    --  " | | "
  where
    m = intVal (Proxy @m)

pprintStringDiagram
  :: forall m n. KnownNat m
  => StringDiagram (Const2 String) m n
  -> [String]
pprintStringDiagram = \case
  Id
    -> [pprint1Pipes m]
  step :>>> qs
    -> withKnownStepSize step $ \_ _
    -> [pprint1Pipes m]
    ++ pprintStep step
    ++ pprintStringDiagram qs
  where
    m = intVal (Proxy @m)


test :: IO ()
test = do
  Some2 _ _ qs <- runRandom $ pickSome2StringDiagram 6
  mapM_ putStrLn $ pprintStringDiagram qs
