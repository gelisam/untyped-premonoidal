{-# LANGUAGE TypeApplications #-}
module UntypedPremonoidal.StringDiagram.Linear where

import UntypedPremonoidal.KnownSize
import UntypedPremonoidal.PPrint
import UntypedPremonoidal.PickGiven
import UntypedPremonoidal.Random
import UntypedPremonoidal.Widen
import UntypedPremonoidal.StringDiagram


data Swap = Swap

type LinearStep
  = Swap

type Linear a
  = StringDiagram LinearStep a

instance KnownSize Swap where
  knownSize Swap
    = (2, 2)

instance KnownSizeGiven Swap

instance PickGiven Swap where
  pickingsGiven m
    = [ pickWidenGiven Swap m
      | m >= 2
      ]

--   [ | | ]
-- > [  X  ]
--   [ | | ]
instance PPrint Swap where
  pprint Swap
    = ["  X  "]

instance PPrintGiven Swap


printRandomLinear :: IO ()
printRandomLinear = do
  (m, as) <- runRandom $ do
    m <- pickFrom [0..5]
    as <- pickStringDiagramGiven @LinearStep 6 m
    pure (m, as)
  mapM_ putStrLn $ pprintGiven as m
