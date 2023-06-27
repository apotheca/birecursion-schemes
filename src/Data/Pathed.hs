module Data.Pathed where

import Data.Bifunctor
import Data.Function
import Data.Functor
import Data.Indexed
import Data.Maybe
import Data.Recursive
import Data.Tuple

import Control.Applicative
import Control.Monad

import Data.Sourced

import Prelude (undefined, concat, concatMap, Foldable(..), print, IO(..), Show(..))

--
-- Indexed morphisms
--

-- NOTE: You might as well use cata / hylo + (alg . indexed) but these are here for completion
icata
    :: (Recursive a, IndexedFunctor (Base a))
    => (Base a (Index (Base a), b) -> b)
    -> a -> b
icata alg = ihylo alg project
-- Or: cata (alg . indexed)

-- -- NOTE: curry (second h) = \ i a -> (i, h a)
ihylo
    :: IndexedFunctor f
    => (f (Index f, b) -> b)
    -> (a -> f a)
    -> a -> b
ihylo alg coalg = h where h = alg . imap (curry (second h)) . coalg
-- Or: hylo (alg . indexed)

-- NOTE: Compare to these -Maybe versions, which are a different way of performing
--  the same thing

icataMaybe
    :: (IndexedFunctor (Base a), Recursive a)
    => (Maybe (Index (Base a)) -> Base a b -> b)
    -> a -> b
icataMaybe alg = pcata (const . Just) alg Nothing

ihyloMaybe
    :: (IndexedFunctor (Base a), Recursive a)
    => (Maybe (Index (Base a)) -> Base a b -> b)
    -> (a -> Base a a)
    -> a -> b
ihyloMaybe alg coalg = phylo (const . Just) alg (const coalg) Nothing

--
-- Pathed mapping
--

pmap
    :: (IndexedFunctor f)
    => (Index f -> path -> path)     -- Cons
    -> (path -> a -> b)
    -> path                           -- Nil
    -> f a -> f b
pmap = smap

ptraverse
    :: (IndexedTraversable t, Applicative f)
    => (Index t -> path -> path)
    -> (path -> a -> f b)
    -> path -> t a -> f (t b)
ptraverse = straverse

--
-- Pathed morphisms
--

pcata
    :: (IndexedFunctor (Base a), Recursive a)
    => (Index (Base a) -> path -> path)
    -> (path -> Base a b -> b)
    -> path
    -> a -> b
pcata = scata id

-- Frag / shard example 
{-
import Data.Map
import Data.Ord

type Path r = [Index (Base r)]
type Frag r = Base r (Path r)
type Shard r = (Path r, Frag r)

shatterList :: (Recursive r, IndexedFunctor (Base r), Foldable (Base r)) => r -> [Shard r]
shatterList r = snd $ pcata (:) shatter [] r where
    -- shatter :: Path r -> Base r (Path r, [Shard r]) -> (Path r, [Shard r])
    shatter p fs = (p, (p, fmap fst fs) : concatMap snd fs)

shatterMap
    :: (Recursive r, IndexedFunctor (Base r), Foldable (Base r), Ord (Index (Base r)))
    => r -> Map (Path r) (Frag r)
shatterMap = fromList . shatterList
-}

pana
    :: (IndexedFunctor (Base b), Corecursive b)
    => (Index (Base b) -> path -> path)
    -> (path -> a -> Base b a)
    -> path
    -> a -> b
pana = sana id

phylo
    :: (IndexedFunctor f)
    => (Index f -> path -> path)
    -> (path -> f b -> b)
    -> (path -> a -> f a)
    -> path
    -> a -> b
phylo = shylo id

-- Trivial, piso = const iso
piso
    :: (Iso a b, IndexedFunctor (Base a))
    => (Index (Base a) -> path -> path)
    -> path
    -> a -> b
piso = siso id

-- NOTE: Inferring the type *will forget* the forall
ptrans
    :: (Trans s t, IndexedFunctor (Base s))
    => (Index (Base s) -> path -> path)
    -> (path -> forall a. Base s a -> Base t a) -- Should this forall be before 'path ->'?
    -> path
    -> s -> t
ptrans = strans id

--
-- Monadic pathed morphisms
--

pcataM
    :: (Monad m, IndexedTraversable (Base a), Recursive a)
    => (Index (Base a) -> path -> path)
    -> (path -> Base a b -> m b)
    -> path
    -> a -> m b
pcataM = scataM id

panaM
    :: (Monad m, IndexedTraversable (Base b), Corecursive b)
    => (Index (Base b) -> path -> path)
    -> (path -> a -> m (Base b a))
    -> path
    -> a -> m b
panaM = sanaM id

phyloM
    :: (Monad m, IndexedTraversable f)
    => (Index f -> path -> path)
    -> (path -> f b -> m b)
    -> (path -> a -> m (f a))
    -> path
    -> a -> m b
phyloM = shyloM id

-- Trivial, pisoM = const isoM
pisoM :: (Monad m, Iso a b, IndexedTraversable (Base b))
    => (Index (Base a) -> path -> path)
    -> path
    -> a -> m b
pisoM = sisoM id

-- NOTE: Inferring the type *will forget* the forall
ptransM
    :: (Monad m, Trans s t, IndexedTraversable (Base s))
    => (Index (Base s) -> path -> path)
    -> (path -> forall a . Base s a -> m (Base t a)) -- Should this forall be before 'path ->'?
    -> path
    -> s -> m t
ptransM = stransM id
