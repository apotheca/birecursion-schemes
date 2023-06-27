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

import Data.Sourced

--
-- Biindexed bimporphisms
--

ibicata
    :: (BiindexedBifunctor (Bibase a), Birecursive a)
    => (Bibase a (FirstIndex (Bibase a), b) (SecondIndex (Bibase a), b) -> b)
    -> a -> b
ibicata alg = ibihylo alg biproject

ibihylo
    :: (BiindexedBifunctor f)
    => (f (FirstIndex f, b) (SecondIndex f, b) -> b) -> (a -> f a a)
    -> a -> b
ibihylo alg coalg = h where h = alg . ibimap (curry (second h)) (curry (second h)) . coalg
-- Or: ibihylo alg = bihylo (alg . biindexed)

-- TODO: ibicataMaybe, ibihyloMaybe

--
-- Bipathed bimapping
--

pbimap
    :: (BiindexedBifunctor f)
    => (FirstIndex f -> path -> path)   -- Left Cons
    -> (SecondIndex f -> path -> path)  -- Right Cons
    -> (path -> a -> b)
    -> (path -> c -> d)
    -> path                             -- Nil
    -> f a c -> f b d
pbimap = sbimap

pbitraverse
    :: (Applicative f, BiindexedBitraversable t)
    => (FirstIndex t -> path -> path)   -- Left Cons
    -> (SecondIndex t -> path -> path)  -- Right Cons
    -> (path -> a -> f b)
    -> (path -> c -> f d)
    -> path                             -- Nil
    -> t a c -> f (t b d)
pbitraverse = sbitraverse

--
-- Bipathed bimorphisms
--

pbicata
    :: (Birecursive a, BiindexedBifunctor (Bibase a)) 
    => (FirstIndex (Bibase a) -> path -> path)
    -> (SecondIndex (Bibase a) -> path -> path)
    -> (path -> Bibase a b b -> b)
    -> path
    -> a -> b
pbicata = sbicata id

pbiana
    :: (Cobirecursive b, BiindexedBifunctor (Bibase b)) 
    => (FirstIndex (Bibase b) -> path -> path)
    -> (SecondIndex (Bibase b) -> path -> path)
    -> (path -> a -> Bibase b a a)
    -> path
    -> a -> b
pbiana = sbiana id

pbihylo
    :: (BiindexedBifunctor f)
    => (FirstIndex f -> path -> path)   -- Left cons
    -> (SecondIndex f -> path -> path)  -- Right cons
    -> (path -> f b b -> b)
    -> (path -> a -> f a a)
    -> path                             -- End's argument
    -> a -> b
pbihylo = sbihylo id

-- Trivial
pbiiso
    :: (Biiso a b, BiindexedBifunctor (Bibase a))
    => (FirstIndex (Bibase a) -> path -> path)
    -> (SecondIndex (Bibase a) -> path -> path)
    -> path
    -> a -> b
pbiiso = sbiiso id

pbitrans
    :: (Bitrans s t, BiindexedBifunctor (Bibase s)) 
    => (FirstIndex (Bibase s) -> path -> path)
    -> (SecondIndex (Bibase s) -> path -> path)
    -> (path -> forall a b . Bibase s a b -> Bibase t a b)
    -> path
    -> s -> t
pbitrans = sbitrans id

--
-- Monadic bipathed bimorphisms
--

pbicataM
    :: (Monad m, Birecursive a, BiindexedBitraversable (Bibase a)) 
    => (FirstIndex (Bibase a) -> path -> path)
    -> (SecondIndex (Bibase a) -> path -> path)
    -> (path -> Bibase a b b -> m b)
    -> path
    -> a -> m b
pbicataM = sbicataM id

pbianaM
    :: (Monad m, Cobirecursive b, BiindexedBitraversable (Bibase b)) 
    => (FirstIndex (Bibase b) -> path -> path)
    -> (SecondIndex (Bibase b) -> path -> path)
    -> (path -> a -> m (Bibase b a a))
    -> path
    -> a -> m b
pbianaM = sbianaM id

pbihyloM
    :: (Monad m, BiindexedBitraversable f)
    => (FirstIndex f -> path -> path)   -- Left cons
    -> (SecondIndex f -> path -> path)  -- Right cons
    -> (path -> f b b -> m b)
    -> (path -> a -> m (f a a))
    -> path                             -- End's argument
    -> a -> m b
pbihyloM = sbihyloM id

-- Trivial
pbiisoM
    :: (Monad m, Biiso a b, BiindexedBitraversable (Bibase a))
    => (FirstIndex (Bibase a) -> path -> path)
    -> (SecondIndex (Bibase a) -> path -> path)
    -> path
    -> a -> m b
pbiisoM = sbiisoM id

pbitransM
    :: (Monad m, Bitrans s t, BiindexedBitraversable (Bibase s)) 
    => (FirstIndex (Bibase s) -> path -> path)
    -> (SecondIndex (Bibase s) -> path -> path)
    -> (path -> forall a b . Bibase s a b -> m (Bibase t a b))
    -> path
    -> s -> m t
pbitransM = sbitransM id
