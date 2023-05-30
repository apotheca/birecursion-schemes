module Data.Pathed where

import Data.Bifunctor
import Data.Function
import Data.Functor
import Data.Indexed
import Data.Recursive
import Data.Tuple

import Control.Applicative
import Control.Monad

import Prelude (undefined, concat, concatMap, Foldable(..), print, IO(..), Show(..))

type Path (r :: *) = [Index (Base r)]

type PathedRecursive a = (Recursive a, IndexedFunctor (Base a))
type PathedCorecursive a = (Corecursive a, IndexedFunctor (Base a))

icata :: (Recursive a, IndexedFunctor (Base a)) => (Base a (Index (Base a), b) -> b) -> a -> b
-- icata alg = cata (alg . indexed)
icata alg = ihylo alg project

-- iana :: (Corecursive b, IndexedFunctor (Base b)) => (a -> Base b a) -> a -> b
-- -- iana = hylo embed . fmap (fmap snd)
-- iana coalg = ihylo (embed . fmap snd) coalg

-- -- NOTE: curry (second h) = \ i a -> (i, h a)
ihylo :: IndexedFunctor f => (f (Index f, b) -> b) -> (a -> f a) -> a -> b
-- ihylo alg = hylo (alg . indexed)
ihylo alg coalg = h where h = alg . imap (curry (second h)) . coalg

pcata :: (PathedRecursive a) => (Path a -> Base a b -> b) -> Path a -> a -> b
pcata alg  = phylo alg (const project)

-- Frag / shard example
{-
type Frag r = Base r (Path r)
type Shard r = (Path r, Frag r)

shatterList :: (Recursive r, IndexedFunctor (Base r), Foldable (Base r)) => r -> [Shard r]
shatterList r = snd $ pcata shatter [] r where
    -- shatter :: Path r -> Base r (Path r, [Shard r]) -> (Path r, [Shard r])
    shatter p fs = (p, (p, fmap fst fs) : concatMap snd fs)

shatterMap :: (Recursive r, IndexedFunctor (Base r), Foldable (Base r)) => r -> Map (Path r) (Frag r)
shatterMap = fromList . shatterList
-}

pana :: (PathedCorecursive b) => (Path b -> a -> Base b a) -> Path b -> a -> b
pana coalg  = phylo (const embed) coalg

-- p-hylo as in pathed-hylo
phylo :: IndexedFunctor f => ([Index f] -> f b -> b) -> ([Index f] -> a -> f a) -> [Index f] -> a -> b
-- phylo alg coalg = h where h p = alg p . imap (\ i -> h (i : p)) . coalg p
phylo alg coalg = h where h p = alg p . imapWith (:) h p . coalg p

-- Non-trivial?
-- pcataA
--     :: (Applicative f, Recursive a, IndexedTraversable (Base a))
--     => (Path a -> Base a b -> b)
--     -> Path a -> a -> f b
-- pcataA alg = phyloA alg (const project)

-- -- Non-trivial?
-- panaA
--     :: (Applicative f, Corecursive b, IndexedTraversable (Base b))
--     => (Path b -> a -> Base b a)
--     -> Path b -> a -> f b
-- panaA coalg = phyloA (const embed) coalg

-- -- Non-trivial?
-- phyloA
--     :: (Applicative f, IndexedTraversable t)
--     => ([Index t] -> t b -> b)
--     -> ([Index t] -> a -> t a)
--     -> [Index t] -> a -> f b
-- phyloA alg coalg = h where
--     h p = fmap (alg p) . itraverse (\ i -> h (i : p)) . coalg p

pcataM
    :: (PathedRecursive a, IndexedTraversable (Base a), Monad m)
    => (Path a -> Base a b -> m b)
    -> Path a -> a -> m b
pcataM alg  = phyloM alg (const projectM)

panaM
    :: (PathedCorecursive b, IndexedTraversable (Base b), Monad m)
    => (Path b -> a -> m (Base b a))
    -> Path b -> a -> m b
panaM coalg  = phyloM (const embedM) coalg

phyloM
    :: (Monad m, IndexedTraversable t)
    => ([Index t] -> t b -> m b)
    -> ([Index t] -> a -> m (t a))
    -> [Index t] -> a -> m b
-- phyloM alg coalg = h where h p = alg p <=< itraverse (\ i -> h (i:p)) <=< coalg p
phyloM alg coalg = h where h p = alg p <=< itraverseWith (:) h p <=< coalg p

-- Trivial
piso :: (Iso s t, IndexedFunctor (Base s)) => Path s -> s -> t
piso = phylo (const embed) (const project)

-- Non-trivial
ptrans :: (PathedRecursive s, Corecursive t) => (Path s -> forall a. Base s a -> Base t a) -> Path s -> s -> t
ptrans f = phylo (\ p -> embed . f p) (const project)

-- Trivial because pisoM = const isoM
pisoM :: (Monad m, Iso s t, IndexedTraversable (Base s)) => Path s -> s -> m t
pisoM = phyloM (const embedM) (const projectM)

ptransM :: (Monad m, PathedRecursive s, Corecursive t, IndexedTraversable (Base s)) => (Path s -> forall a. Base s a -> m (Base t a)) -> Path s -> s -> m t
ptransM f = phyloM (\ p -> embedM <=< f p) (const projectM)
