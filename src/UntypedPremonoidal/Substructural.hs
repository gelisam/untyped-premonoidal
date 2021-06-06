{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module UntypedPremonoidal.Substructural where

import Control.Category ((>>>))
import Data.Void (Void, absurd)


class Substructural a where
  restructure
    :: a -> [e] -> [e]

instance Substructural a => Substructural [a] where
  restructure []
    = id
  restructure (a:as)
      = restructure a
    >>> restructure as

instance Substructural Void where
  restructure
    = absurd

instance (Substructural a, Substructural b) => Substructural (Either a b) where
  restructure (Left a)
    = restructure a
  restructure (Right b)
    = restructure b
