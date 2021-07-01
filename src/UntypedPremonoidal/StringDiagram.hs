{-# LANGUAGE AllowAmbiguousTypes, DeriveFunctor, FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module UntypedPremonoidal.StringDiagram where

import Control.Monad (when)
import Data.Dynamic (Dynamic, fromDyn, toDyn)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import UntypedPremonoidal.Atom
import UntypedPremonoidal.KnownSize
import UntypedPremonoidal.Interpret
import UntypedPremonoidal.PPrint
import UntypedPremonoidal.Random
import UntypedPremonoidal.Substructural
import UntypedPremonoidal.Widen
import UntypedPremonoidal.WidenPickings


newtype StringDiagram step a = StringDiagram
  { unStringDiagram :: [Widen (step `Either` a)]
  }
  deriving (Functor)


instance ( Substructural step
         , Interpret a
         )
      => Interpret (StringDiagram step a)
         where
  interpret
    = interpret
    . unStringDiagram


instance ( KnownSizeGiven step
         , KnownSizeGiven a
         )
      => KnownSizeGiven (StringDiagram step a) where
  knownSizeGiven
    = knownSizeGiven
    . unStringDiagram

pickStringDiagramGiven
  :: forall step a
   . ( KnownSizeGiven step, KnownSizeGiven a
     , WidenPickings step, WidenPickings a
     , PickAtom a
     )
  => Int  -- desired length
  -> (Int -> Random (StringDiagram step a))
pickStringDiagramGiven desiredLength m = do
  as <- pickListGiven desiredLength pickGiven m
  pure $ StringDiagram as


instance ( PPrintGiven step
         , PPrintGiven a
         )
      => PPrintGiven (StringDiagram step a) where
  pprintGiven
    = pprintGiven
    . unStringDiagram

printRandomStringDiagram
  :: forall step
   . ( PPrintGiven step
     , WidenPickings step
     )
  => IO ()
printRandomStringDiagram = do
  (m, as) <- runRandom $ do
    m <- pickFrom [0..5]
    as <- pickStringDiagramGiven @step @(Atom String) 6 m
    pure (m, as)
  mapM_ putStrLn $ pprintGiven as m


normalizeRandomStringDiagram
  :: forall step
   . ( Substructural step
     , PPrintGiven step
     , WidenPickings step
     )
  => ( forall a
     . StringDiagram step a
    -> (Int -> StringDiagram step a)
     )
  -> IO ()
normalizeRandomStringDiagram normalizeGiven = do
  (m, as) <- runRandom $ do
    m <- pickFrom [0..5]
    as <- pickStringDiagramGiven @step @(Atom String) 6 m
    pure (m, as)
  let as'
        :: StringDiagram step (Atom String)
      as'
        = normalizeGiven as m
  mapM_ putStrLn $ sideEqualSide (pprintGiven as m)
                                 (pprintGiven as' m)

  -- check that the diagrams are really equivalent
  let asString
        :: Dynamic -> String
      asString dynamic
        = fromDyn dynamic (error "expected String")
      asStrings
        :: Seq Dynamic -> Seq String
      asStrings
        = fmap asString
      asDynamics
        :: Seq String -> Seq Dynamic
      asDynamics
        = fmap toDyn
      asStringsFun
        :: (Seq Dynamic -> Seq Dynamic)
        -> (Seq String -> Seq String)
      asStringsFun f
        = asStrings . f . asDynamics
      asDynamicsFun
        :: (Seq String -> Seq String)
        -> (Seq Dynamic -> Seq Dynamic)
      asDynamicsFun f
        = asDynamics . f . asStrings
  let fs, fs'
        :: StringDiagram step (Seq Dynamic -> Seq Dynamic)
      fs
        = fmap (asDynamicsFun . toInjectiveFunction) as
      fs'
        = fmap (asDynamicsFun . toInjectiveFunction) as'
  let inputs
        :: Seq String
      inputs
        = Seq.fromList
        $ [ "x" ++ show i
          | i <- [1..m]
          ]
  let outputs, outputs'
        :: Seq String
      outputs
        = asStringsFun (interpret fs) inputs
      outputs'
        = asStringsFun (interpret fs') inputs
  when (outputs /= outputs') $ do
    print outputs
    putStrLn "/="
    print outputs'
    error "diagrams are not equivalent"
