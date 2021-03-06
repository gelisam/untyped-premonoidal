{-# LANGUAGE DefaultSignatures, FlexibleInstances, MultiParamTypeClasses #-}
module UntypedPremonoidal.Interpret where

import Control.Category ((>>>))
import Data.Dynamic (Dynamic)
import Data.Sequence (Seq)
import Data.Void (Void)

import UntypedPremonoidal.Substructural


class Interpret a where
  interpret
    :: a -> Seq Dynamic -> Seq Dynamic

  default interpret
    :: Substructural a
    => a -> Seq Dynamic -> Seq Dynamic
  interpret
    = restructure

instance Interpret a => Interpret [a] where
  interpret []
    = id
  interpret (a:as)
      = interpret a
    >>> interpret as

instance Interpret Void

instance (Substructural a, Interpret b) => Interpret (Either a b) where
  interpret (Left a)
    = restructure a
  interpret (Right b)
    = interpret b

instance Interpret (Seq Dynamic -> Seq Dynamic) where
  interpret f
    = f
