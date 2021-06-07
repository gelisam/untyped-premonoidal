{-# LANGUAGE LambdaCase, TypeApplications, TypeOperators #-}
module UntypedPremonoidal.StringDiagram.Cartesian where

import UntypedPremonoidal.Interpret
import UntypedPremonoidal.KnownSize
import UntypedPremonoidal.PPrint
import UntypedPremonoidal.StringDiagram
import UntypedPremonoidal.StringDiagram.Affine
import UntypedPremonoidal.StringDiagram.Linear
import UntypedPremonoidal.Substructural
import UntypedPremonoidal.Widen
import UntypedPremonoidal.WidenPickings


data Dup = Dup

type CartesianStep
  = Swap `Either` Drop `Either` Dup

type Cartesian a
  = StringDiagram CartesianStep a


instance Substructural Dup where
  restructure Dup = \case
    [x]
      -> [x, x]
    xs
      -> error $ "restructure Dup: input should have length 1, "
              ++ "but the given list has length "
              ++ show (length xs)

instance Interpret Dup


instance KnownSize Dup where
  knownSize Dup
    = (1, 2)

instance KnownSizeGiven Dup


instance WidenPickings Dup where
  widenPickingsGiven m
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
printRandomCartesian
  = printRandomStringDiagram @CartesianStep
