{-# LANGUAGE TypeApplications #-}
module UntypedPremonoidal.StringDiagram.Premonoidal where

import Data.Void (Void)

import UntypedPremonoidal.StringDiagram


type PremonoidalStep
  = Void

type Premonoidal a
  = StringDiagram PremonoidalStep a


printRandomPremonoidal :: IO ()
printRandomPremonoidal
  = printRandomStringDiagram @PremonoidalStep


normalizePremonoidalGiven
  :: Premonoidal a
  -> (Int -> Premonoidal a)
normalizePremonoidalGiven as _
  = as  -- Every term is already in normal form!

normalizeRandomPremonoidal :: IO ()
normalizeRandomPremonoidal
  = normalizeRandomStringDiagram normalizePremonoidalGiven
