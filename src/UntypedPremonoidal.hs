{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeOperators #-}
module UntypedPremonoidal where

import Data.Kind
import Data.Proxy
import GHC.TypeLits


data StringDiagram (q :: Nat -> Nat -> Type)
                   (m :: Nat)
                   (n :: Nat)
                   where
  Atom
    :: (KnownNat m, KnownNat n)
    => q m n
    -> StringDiagram q m n
  Id
    :: KnownNat n
    => StringDiagram q n n
  (:>>>)
    :: StringDiagram q m n
    -> StringDiagram q n o
    -> StringDiagram q m o
  Widen
    :: (KnownNat pre, KnownNat post)
    => Proxy pre
    -> StringDiagram q m n
    -> Proxy post
    -> StringDiagram q (pre + m + post)
                       (pre + n + post)
  Swap
    :: StringDiagram q 2 2
  Drop
    :: StringDiagram q 1 0
  Dup
    :: StringDiagram q 1 2
