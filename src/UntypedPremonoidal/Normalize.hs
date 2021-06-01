{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}
module UntypedPremonoidal.Normalize where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, execStateT, get, modify, put, runStateT)

import UntypedPremonoidal.KnownSize
import UntypedPremonoidal.Name
import UntypedPremonoidal.Widen


-- ([Name], [Name]):
--   Two permutations of the same list of distinct wire names, indicating the
--   order of the wires after having applied the non-canonical morphisms which
--   have already been consumed, and after having applied the canonical
--   morphisms which have been added to the morphism list, respectively.
--
-- [morphism]:
--   The canonical morphisms which have been emitted so far. To make appending
--   morphisms at the end efficient, the list is stored in reverse order.
newtype Normalize morphism a = Normalize
  { unNormalize :: StateT ([Int], [Int])
                 ( StateT [morphism]
                   NameGen
                 ) a
  }
  deriving (Functor, Applicative, Monad)

runNormalizeGiven
  :: Normalize morphism ()
  -> (Int -> [morphism])
runNormalizeGiven body m
  = reverse
  . runNameGen
  . flip execStateT []
  . flip evalStateT ([0..m-1], [0..m-1])
  . unNormalize
  $ body

getWires
  :: Normalize morphism ([Int], [Int])
getWires = Normalize $ do
  get

consumeNonCanonicalMorphism
  :: morphism
  -> ([Int] -> [Int])
  -> Normalize morphism ()
consumeNonCanonicalMorphism _morphism effectOnWires = Normalize $ do
  (xs, ys) <- get
  let xs' = effectOnWires xs
  put (xs', ys)

addCanonicalMorphism
  :: KnownSize morphism
  => morphism
  -> ([Int] -> [Int])
  -> Normalize (Widen morphism) ()
addCanonicalMorphism morphism effectOnWires = Normalize $ do
  (xs, ys) <- get
  let ys' = effectOnWires ys
  put (xs, ys')
  let m = length xs
  let (m', _n') = knownSize morphism
  let post = m - m'
  lift $ modify (Widen 0 morphism post :)

genWireName
  :: Normalize morphism Name
genWireName = Normalize $ do
  lift $ lift $ genName

withoutFirstWire
  :: Normalize (Widen morphism) a
  -> Normalize (Widen morphism) a
withoutFirstWire body = Normalize $ do
  get >>= \case
    (x:xs, y:ys) -> do
      ((a, (xs', ys')), morphisms)
        <- lift $ lift
         $ flip runStateT []
         $ flip runStateT (xs, ys)
         $ unNormalize
         $ body
      put (x:xs', y:ys')
      lift $ modify (fmap addFirstWire morphisms ++)
      pure a
    _ -> do
      error "withoutFirstWire: there are currently zero wires"
  where
    addFirstWire
      :: Widen morphism -> Widen morphism
    addFirstWire (Widen pre morphism post)
      = Widen (pre + 1) morphism post
