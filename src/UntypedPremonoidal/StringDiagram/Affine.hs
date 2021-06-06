{-# LANGUAGE LambdaCase, TypeApplications, TypeOperators #-}
module UntypedPremonoidal.StringDiagram.Affine where

import UntypedPremonoidal.Interpret
import UntypedPremonoidal.KnownSize
import UntypedPremonoidal.PPrint
import UntypedPremonoidal.Random
import UntypedPremonoidal.StringDiagram
import UntypedPremonoidal.StringDiagram.Linear
import UntypedPremonoidal.Substructural
import UntypedPremonoidal.Widen
import UntypedPremonoidal.WidenPickings


data Drop = Drop

type AffineStep
  = Swap `Either` Drop

type Affine a
  = StringDiagram AffineStep a


instance Substructural Drop where
  restructure Drop = \case
    [_]
      -> []
    xs
      -> error $ "restructure Drop: input should have length 1, "
              ++ "but the given list has length "
              ++ show (length xs)

instance Interpret Drop


instance KnownSize Drop where
  knownSize Drop
    = (1, 0)

instance KnownSizeGiven Drop


instance WidenPickings Drop where
  widenPickingsGiven m
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
    as <- pickStringDiagramGiven @AffineStep @String 6 m
    pure (m, as)
  mapM_ putStrLn $ pprintGiven as m
