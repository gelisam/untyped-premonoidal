{-# LANGUAGE ScopedTypeVariables, TypeOperators #-}
module UntypedPremonoidal.StringDiagram where

import UntypedPremonoidal.Atom
import UntypedPremonoidal.KnownSize
import UntypedPremonoidal.PickGiven
import UntypedPremonoidal.Random
import UntypedPremonoidal.Widen


type StringDiagram step q
  = [Widen (step `Either` Atom q)]


pickStringDiagramGiven
  :: forall step
   . (PickGiven step, KnownSizeGiven step)
  => Int  -- desired length
  -> (Int -> Random (StringDiagram step String))
pickStringDiagramGiven desiredLength
  = pickListGiven desiredLength pickGiven
