{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses #-}
module UntypedPremonoidal.Widen where

import UntypedPremonoidal.Interpret
import UntypedPremonoidal.KnownSize
import UntypedPremonoidal.PPrint
import UntypedPremonoidal.Random
import UntypedPremonoidal.Substructural


data Widen a = Widen
  { widenPre :: Int
  , widenMid :: a
  , widenPost :: Int
  }
  deriving Functor


instance Substructural a => Substructural (Widen a) where
  restructure (Widen pre a post) preMidPost
    = let (_pre, midPost) = splitAt pre preMidPost
          (mid, _post) = splitAt (length midPost - post) midPost
      in restructure a mid

instance Interpret a => Interpret (Widen a) where
  interpret (Widen pre a post) preMidPost
    = let (_pre, midPost) = splitAt pre preMidPost
          (mid, _post) = splitAt (length midPost - post) midPost
      in interpret a mid


instance KnownSize a => KnownSize (Widen a) where
  knownSize (Widen pre a post)
    = let (m', n') = knownSize a
   in ( pre + m' + post
      , pre + n' + post
      )

instance KnownSizeGiven a => KnownSizeGiven (Widen a) where
  knownSizeGiven (Widen pre a post) m
    = knownSizeGiven a (m - pre - post) + pre + post


pickWidenGiven
  :: KnownSize a
  => a
  -> (Int -> Random (Widen a))
pickWidenGiven a m = do
  let (m', _n') = knownSize a
  let extraM = m - m'
  pre <- pickFrom [0..extraM]
  let post = extraM - pre
  pure $ Widen pre a post


--     [ | | | ]
-- >   [/ / /  ]
--   [ | | | ]
pprint1Slashes
  :: Int -> String
pprint1Slashes n
  = concat (replicate n "/ ")
 ++ " "

--   [ | | | ]
-- > [  \ \ \]
--     [ | | | ]
pprint1Backslashes
  :: Int -> String
pprint1Backslashes n
  = " "
 ++ concat (replicate n " \\")

--   [ | | ]
-- > [  \ \]
-- > [+-+| | ]
-- > [|f|| | ]
-- > [+-+| | ]
-- > [  / /  ]
--   [ | | ]
pprintWiden
  :: Int  -- input size
  -> Widen [String]
  -> Int  -- output size
  -> [String]
pprintWiden m (Widen pre pprintedA post) n
    = let m' = m - pre - post
          n' = n - pre - post
          w' = 1 `max` m' `max` n'
   in [ pprint1Pipes (pre + m')
    .+. pprint1Spaces blanks
    .+. pprint1Backslashes post
      | let gap = w' - m'
      , blanks <- [0..gap-1]
      , post > 0
      ]
   ++ [ pprint1Pipes pre
    .+. s
    .+. pprint1Pipes post
      | s <- pprintedA
      ]
   ++ [ pprint1Pipes (pre + n')
    .+. pprint1Spaces blanks
    .+. pprint1Slashes post
      | let gap = w' - n'
      , blanks <- [gap,gap-1..1]
      , post > 0
      ]

instance PPrint a => PPrint (Widen a) where
  pprint (Widen pre a post)
    = let (m', n') = knownSize a
   in pprintWiden
        (pre + m' + post)
        (Widen pre (pprint a) post)
        (pre + n' + post)

instance PPrintGiven a => PPrintGiven (Widen a) where
  pprintGiven (Widen pre a post) m
    = let m' = m - pre - post
          n' = knownSizeGiven a m'
          n = pre + n' + post
   in pprintWiden
        m
        (Widen pre (pprintGiven a m') post)
        n
