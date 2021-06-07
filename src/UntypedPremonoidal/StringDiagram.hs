{-# LANGUAGE DeriveFunctor, FlexibleContexts, ScopedTypeVariables, TypeOperators #-}
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
  { unStringDiagram :: [Widen (step `Either` Atom a)]
  }
  deriving (Functor)


instance ( Substructural step
         , Interpret (Atom a)
         )
      => Interpret (StringDiagram step a)
         where
  interpret
    = interpret
    . unStringDiagram


instance KnownSizeGiven step
      => KnownSizeGiven (StringDiagram step a) where
  knownSizeGiven
    = knownSizeGiven
    . unStringDiagram


instance ( PPrintGiven step
         , PPrintGiven (Atom a)
         )
      => PPrintGiven (StringDiagram step a) where
  pprintGiven
    = pprintGiven
    . unStringDiagram


pickStringDiagramGiven
  :: forall step a
   . (KnownSizeGiven step, WidenPickings step, PickAtom a)
  => Int  -- desired length
  -> (Int -> Random (StringDiagram step a))
pickStringDiagramGiven desiredLength m = do
  as <- pickListGiven desiredLength pickGiven m
  pure $ StringDiagram as
