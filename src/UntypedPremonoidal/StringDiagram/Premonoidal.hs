{-# LANGUAGE TypeApplications #-}
module UntypedPremonoidal.StringDiagram.Premonoidal where

import Data.Void (Void)

import UntypedPremonoidal.PPrint
import UntypedPremonoidal.Random
import UntypedPremonoidal.StringDiagram


type PremonoidalStep
  = Void

type Premonoidal q
  = StringDiagram PremonoidalStep q


printRandomPremonoidal :: IO ()
printRandomPremonoidal = do
  (m, as) <- runRandom $ do
    m <- pickFrom [0..5]
    as <- pickStringDiagramGiven @PremonoidalStep 6 m
    pure (m, as)
  mapM_ putStrLn $ pprintGiven as m
