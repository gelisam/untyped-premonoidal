{-# LANGUAGE LambdaCase, TypeApplications #-}
module UntypedPremonoidal.StringDiagram.Linear where

import UntypedPremonoidal.Interpret
import UntypedPremonoidal.KnownSize
import UntypedPremonoidal.PPrint
import UntypedPremonoidal.Random
import UntypedPremonoidal.StringDiagram
import UntypedPremonoidal.Substructural
import UntypedPremonoidal.Widen
import UntypedPremonoidal.WidenPickings


data Swap = Swap

type LinearStep
  = Swap

type Linear a
  = StringDiagram LinearStep a


instance Substructural Swap where
  restructure Swap = \case
    [x, y]
      -> [y, x]
    xs
      -> error $ "restructure Swap: input should have length 2, "
              ++ "but the given list has length "
              ++ show (length xs)

instance Interpret Swap


instance KnownSize Swap where
  knownSize Swap
    = (2, 2)

instance KnownSizeGiven Swap


instance WidenPickings Swap where
  widenPickingsGiven m
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
