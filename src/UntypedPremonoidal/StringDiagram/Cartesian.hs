{-# LANGUAGE TypeApplications, TypeOperators #-}
module UntypedPremonoidal.StringDiagram.Cartesian where

import UntypedPremonoidal.KnownSize
import UntypedPremonoidal.PPrint
import UntypedPremonoidal.PickGiven
import UntypedPremonoidal.Random
import UntypedPremonoidal.Widen
import UntypedPremonoidal.StringDiagram
import UntypedPremonoidal.StringDiagram.Affine
import UntypedPremonoidal.StringDiagram.Linear


data Dup = Dup

type CartesianStep
  = Swap `Either` Drop `Either` Dup

type Cartesian a
  = StringDiagram CartesianStep a

instance KnownSize Dup where
  knownSize Dup
    = (1, 2)

instance KnownSizeGiven Dup

instance PickGiven Dup where
  pickingsGiven m
    = [ pickWidenGiven Dup m
      | m >= 1
      ]

--   [ | ]
-- > [ |\  ]
--   [ | | ]
instance PPrint Dup where
  pprint Dup
    = [" |\\  "]

instance PPrintGiven Dup


printRandomCartesian :: IO ()
printRandomCartesian = do
  (m, as) <- runRandom $ do
    m <- pickFrom [0..5]
    as <- pickStringDiagramGiven @CartesianStep 6 m
    pure (m, as)
  mapM_ putStrLn $ pprintGiven as m
