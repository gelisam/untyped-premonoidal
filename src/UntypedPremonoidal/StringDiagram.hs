{-# LANGUAGE AllowAmbiguousTypes, DeriveFunctor, FlexibleContexts, RankNTypes, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module UntypedPremonoidal.StringDiagram where

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
   . ( PPrintGiven step
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
  let as' = normalizeGiven as m
  mapM_ putStrLn $ sideEqualSide (pprintGiven as m)
                                 (pprintGiven as' m)
