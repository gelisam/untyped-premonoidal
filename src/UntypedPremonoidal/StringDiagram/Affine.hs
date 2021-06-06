{-# LANGUAGE LambdaCase, TypeApplications, TypeOperators #-}
module UntypedPremonoidal.StringDiagram.Affine where

import UntypedPremonoidal.KnownSize
import UntypedPremonoidal.PPrint
import UntypedPremonoidal.PickGiven
import UntypedPremonoidal.Random
import UntypedPremonoidal.StringDiagram
import UntypedPremonoidal.StringDiagram.Linear
import UntypedPremonoidal.Substructural
import UntypedPremonoidal.Widen


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
