{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
module UntypedPremonoidal.StringDiagram where

import UntypedPremonoidal.Atom
import UntypedPremonoidal.KnownSize
import UntypedPremonoidal.Random
import UntypedPremonoidal.Widen
import UntypedPremonoidal.WidenPickings


type StringDiagram step q
  = [Widen (step `Either` Atom q)]


pickStringDiagramGiven
  :: forall step a
   . (KnownSizeGiven step, WidenPickings step, PickAtom a)
  => Int  -- desired length
  -> (Int -> Random (StringDiagram step a))
pickStringDiagramGiven desiredLength
  = pickListGiven desiredLength pickGiven
