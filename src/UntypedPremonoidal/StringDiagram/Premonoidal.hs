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



normalizePremonoidalGiven
  :: Premonoidal a
  -> (Int -> Premonoidal a)
normalizePremonoidalGiven as _
  = as  -- Every term is already in normal form!

printRandomPremonoidal :: IO ()
printRandomPremonoidal = do
  (m, as) <- runRandom $ do
    m <- pickFrom [0..5]
    as <- pickStringDiagramGiven @PremonoidalStep 6 m
    pure (m, as)
  mapM_ putStrLn $ pprintGiven as m

normalizeRandomPremonoidal :: IO ()
normalizeRandomPremonoidal = do
  (m, as) <- runRandom $ do
    m <- pickFrom [0..5]
    as <- pickStringDiagramGiven @PremonoidalStep 6 m
    pure (m, as)
  let as' = normalizePremonoidalGiven as m
  mapM_ putStrLn $ sideEqualSide (pprintGiven as m)
                                 (pprintGiven as' m)
