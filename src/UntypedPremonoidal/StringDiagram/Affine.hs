{-# LANGUAGE TypeApplications, TypeOperators #-}
module UntypedPremonoidal.StringDiagram.Affine where

import UntypedPremonoidal.KnownSize
import UntypedPremonoidal.PPrint
import UntypedPremonoidal.PickGiven
import UntypedPremonoidal.Random
import UntypedPremonoidal.Widen
import UntypedPremonoidal.StringDiagram
import UntypedPremonoidal.StringDiagram.Linear


data Drop = Drop

type AffineStep
  = Swap `Either` Drop

instance KnownSize Drop where
  knownSize Drop
    = (1, 0)

instance KnownSizeGiven Drop

instance PickGiven Drop where
  pickingsGiven m
    = [ pickWidenGiven Drop m
      | m >= 1
      ]

--   [ | ]
-- > [ x ]
--   [   ]
instance PPrint Drop where
  pprint Drop
    = [" x "]

instance PPrintGiven Drop


printRandomAffine :: IO ()
printRandomAffine = do
  (m, as) <- runRandom $ do
    m <- pickFrom [0..5]
    as <- pickStringDiagramGiven @AffineStep 6 m
    pure (m, as)
  mapM_ putStrLn $ pprintGiven as m
