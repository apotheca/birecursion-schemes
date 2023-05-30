module Data.Bipathed where

import Prelude (undefined)

import Data.Bifunctor
import Data.Bifoldable
import Data.Biindexed
import Data.Birecursive
import Data.Bitraversable
import Data.Either
import Data.Function
import Data.Functor
import Data.Indexed
import Data.Recursive
import Data.Tuple

import Control.Applicative
import Control.Monad

type Bipath (r :: *) = [Biindex (Bibase r)]

type BipathedBirecursive t = (Birecursive t, BiindexedBifunctor (Bibase t))
type BipathedCobirecursive t = (Cobirecursive t, BiindexedBifunctor (Bibase t))

-- NOTE: Effectively calls fmap twice (once per biindexed, once per bicata)
ibicata :: (BipathedBirecursive t) => (Bibase t (FirstIndex (Bibase t), a) (SecondIndex (Bibase t), a) -> a) -> t -> a
-- ibicata alg = bicata (alg . biindexed)
ibicata alg = ibihylo alg biproject

-- NOTE: Effectively calls fmap twice (once per biindexed, once per bihylo)
ibihylo :: (BiindexedBifunctor f) => (f (FirstIndex f, b) (SecondIndex f, b) -> b) -> (a -> f a a) -> a -> b
-- ibihylo alg = bihylo (alg . biindexed)
-- Fuses the two fmaps, note doesn't have Biindexed f, Bifunctor f - has BiindexedBifunctor still
ibihylo alg coalg = h where h = alg . ibimap (curry (second h)) (curry (second h)) . coalg

pbicata
    :: (BipathedBirecursive a)
    => (Bipath a -> Bibase a b b -> b)
    -> Bipath a -> a -> b
pbicata alg = pbihylo alg (const biproject)

pbiana
    :: (BipathedCobirecursive b) 
    => (Bipath b -> a -> Bibase b a a)
    -> Bipath b -> a -> b
pbiana coalg = pbihylo (const biembed) coalg

pbihylo
    :: (BiindexedBifunctor f)
    => ([Biindex f] -> f b b -> b)
    -> ([Biindex f] -> a -> f a a)
    -> [Biindex f] -> a -> b
-- pbihylo alg coalg = h where h p = alg p . ibimap (\ i -> h (Left i : p)) (\ j -> h (Right j : p)) . coalg p
pbihylo alg coalg = h where h p = alg p . ibimapWith ((:) . Left) ((:) . Right) h h p . coalg p

-- Non-trivial?
pbicataA
    :: (Applicative f, Birecursive t, BiindexedBitraversable (Bibase t))
    => (Bipath t -> Bibase t b b -> b)
    -> Bipath t -> t -> f b
pbicataA alg = pbihyloA alg (const biproject)

-- Non-trivial?
pbianaA
    :: (Applicative f, Cobirecursive t, BiindexedBitraversable (Bibase t))
    => (Bipath t -> a -> Bibase t a a)
    -> Bipath t -> a -> f t
pbianaA coalg = pbihyloA (const biembed) coalg

-- Non-trivial?
pbihyloA
    :: (Applicative f, BiindexedBitraversable t)
    => ([Biindex t] -> t b b -> b)
    -> ([Biindex t] -> a -> t a a)
    -> [Biindex t] -> a -> f b
pbihyloA alg coalg = h where
    h p = fmap (alg p) . ibitraverse (\ i -> h (Left i : p)) (\ i -> h (Right i : p)) . coalg p

pbicataM
    :: (Monad m, Birecursive a, BiindexedBifunctor (Bibase a), BiindexedBitraversable (Bibase a))
    => (Bipath a -> Bibase a b b -> m b)
    -> Bipath a -> a -> m b
pbicataM alg = pbihyloM alg (const biprojectM)

pbianaM
    :: (Monad m, Cobirecursive b, BiindexedBifunctor (Bibase b), BiindexedBitraversable (Bibase b))
    => (Bipath b -> a -> m (Bibase b a a))
    -> Bipath b -> a -> m b
pbianaM coalg = pbihyloM (const biembedM) coalg

pbihyloM
    :: (Monad m, BiindexedBifunctor f, BiindexedBitraversable f)
    => ([Biindex f] -> f b b -> m b)
    -> ([Biindex f] -> a -> m (f a a))
    -> [Biindex f] -> a -> m b
-- pbihyloM alg coalg = h where h p = alg p <=< ibitraverse (\ i -> h (Left i : p)) (\ j -> h (Right j : p)) <=< coalg p
pbihyloM alg coalg = h where h p = alg p <=< ibitraverseWith ((:) . Left) ((:) . Right) h h p <=< coalg p

-- TODO: pbihyloWith / pbihyloMWith

-- Trivial...
pbiiso :: (BipathedBirecursive s, Biiso s t) => Bipath s -> s -> t
pbiiso = pbihylo (const biembed) (const biproject)

-- Non-trivial
pbitrans
    :: (BipathedBirecursive s, Cobirecursive t)
    => (Bipath s -> forall a b. Bibase s a b -> Bibase t a b) -> Bipath s -> s -> t
pbitrans f = pbihylo (\ p -> biembed . f p) (const biproject)

-- Trivial
pbiisoM
    :: (Monad m, BipathedBirecursive s, Biiso s t, BiindexedBitraversable (Bibase s))
    => Bipath s -> s -> m t
pbiisoM = pbihyloM (const biembedM) (const biprojectM)

-- Non-trivial
pbitransM
    :: (Monad m, BipathedBirecursive s, Cobirecursive t, BiindexedBitraversable (Bibase s))
    => (Bipath s -> forall a b. Bibase s a b -> m (Bibase t a b))
    -> Bipath s -> s -> m t
pbitransM f = pbihyloM (\ p -> biembedM <=< f p) (const biprojectM)
