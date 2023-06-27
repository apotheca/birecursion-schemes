module Data.Dipathed where

import Data.Bifunctor
import Data.Biindexed
import Data.Bitraversable
import Data.Direcursive
import Data.Direcursive
import Data.Indexed
import Data.Function
import Data.Functor
import Data.Pathed
import Data.Recursive

import Data.Sourced

import Control.Applicative
import Control.Monad

--
-- Pathed dimorphisms
--

pdicata
    :: (BiindexedBifunctor (Dibase f), Direcursive f)
    => (FirstIndex (Dibase f) -> path -> index)
    -> (SecondIndex (Dibase f) -> path -> path)
    -> (index -> a -> b)
    -> (path -> Dibase f b d -> d)
    -> path
    -> f a -> d
pdicata = sdicata id

pdiana
    :: (BiindexedBifunctor (Dibase f), Codirecursive f)
    => (FirstIndex (Dibase f) -> path -> index)
    -> (SecondIndex (Dibase f) -> path -> path)
    -> (index -> a -> b)
    -> (path -> c -> Dibase f a c)
    -> path
    -> c -> f b
pdiana = sdiana id

pdihylo
    :: (BiindexedBifunctor f)
    => (FirstIndex f -> path -> index)  -- Cap
    -> (SecondIndex f -> path -> path)  -- Cons
    -> (index -> a -> b)
    -> (path -> f b d -> d)
    -> (path -> c -> f a c)
    -> path                             -- Nil
    -> c -> d
pdihylo = sdihylo id

pdiiso
    :: (Diiso s t, BiindexedBifunctor (Dibase s))
    => (FirstIndex (Dibase s) -> path -> index)
    -> (SecondIndex (Dibase s) -> path -> path)
    -> (index -> a -> b)
    -> path
    -> s a -> t b
pdiiso = sdiiso id

pditrans
    :: (Ditrans s t, BiindexedBifunctor (Dibase s))
    => (FirstIndex (Dibase s) -> path -> index)
    -> (SecondIndex (Dibase s) -> path -> path)
    -> (index -> a -> b)
    -> (path -> forall x y .  Dibase s x y -> Dibase t x y)
    -> path -> s a -> t b
pditrans = sditrans id

--
-- Applicative pathed dimorphisms
--

pdicataA
    :: (Applicative f, Direcursive t, BiindexedBitraversable (Dibase t))
    => (FirstIndex (Dibase t) -> path -> index)  -- Cap
    -> (SecondIndex (Dibase t) -> path -> path)  -- Cons
    -> (index -> a -> f b)
    -> (path -> Dibase t b c -> c)
    -> path
    -> t a -> f c
pdicataA = sdicataA id

pdianaA
    :: (Applicative f, Codirecursive t, BiindexedBitraversable (Dibase t))
    => (FirstIndex (Dibase t) -> path -> index)  -- Cap
    -> (SecondIndex (Dibase t) -> path -> path)  -- Cons
    -> (index -> a -> f b)
    -> (path -> c -> Dibase t a c)
    -> path                                      -- Nil
    -> c -> f (t b)
pdianaA = sdianaA id

pdihyloA
    :: (Applicative f, BiindexedBitraversable t)
    => (FirstIndex t -> path -> index)  -- Cap
    -> (SecondIndex t -> path -> path)  -- Cons
    -> (index -> a -> f b)
    -> (path -> t b d -> d)
    -> (path -> c -> t a c)
    -> path                             -- Nil
    -> c -> f d
pdihyloA = sdihyloA id

pdiisoA
    :: (Applicative f, Diiso s t, BiindexedBitraversable (Dibase s))
    => (FirstIndex (Dibase s) -> path -> index)
    -> (SecondIndex (Dibase s) -> path -> path)
    -> (index -> a -> f b)
    -> path
    -> s a -> f (t b)
pdiisoA = sdiisoA id

pditransA
    :: (Applicative f, Ditrans s t, BiindexedBitraversable (Dibase s))
    => (FirstIndex (Dibase s) -> path -> index)
    -> (SecondIndex (Dibase s) -> path -> path)
    -> (index -> a -> f b)
    -> (path -> forall x y . Dibase s x y -> Dibase t x y)
    -> path
    -> s a -> f (t b)
pditransA = sditransA id

--
-- Monadic pathed dimorphisms
--

pdicataM
    :: (Monad m, BiindexedBitraversable (Dibase f), Direcursive f)
    => (FirstIndex (Dibase f) -> path -> index)
    -> (SecondIndex (Dibase f) -> path -> path)
    -> (index -> a -> m b)
    -> (path -> Dibase f b d -> m d)
    -> path
    -> f a -> m d
pdicataM = sdicataM id

pdianaM
    :: (Monad m, BiindexedBitraversable (Dibase f), Codirecursive f)
    => (FirstIndex (Dibase f) -> path -> index)
    -> (SecondIndex (Dibase f) -> path -> path)
    -> (index -> a -> m b)
    -> (path -> c -> m (Dibase f a c))
    -> path
    -> c -> m (f b)
pdianaM = sdianaM id

pdihyloM
    :: (Monad m, BiindexedBitraversable f)
    => (FirstIndex f -> path -> index)
    -> (SecondIndex f -> path -> path)
    -> (index -> a -> m b)
    -> (path -> f b d -> m d)
    -> (path -> c -> m (f a c))
    -> path
    -> c -> m d
pdihyloM = sdihyloM id

pdiisoM
    :: (Monad m, Diiso s t, BiindexedBitraversable (Dibase s)) 
    => (FirstIndex (Dibase s) -> path -> index)
    -> (SecondIndex (Dibase s) -> path -> path)
    -> (index -> a -> m b)
    -> path
    -> s a -> m (t b)
pdiisoM = sdiisoM id

pditransM
    :: (Monad m, Ditrans s t, BiindexedBitraversable (Dibase s))
    => (FirstIndex (Dibase s) -> path -> index)
    -> (SecondIndex (Dibase s) -> path -> path)
    -> (index -> a -> m b)
    -> (path -> forall x y . Dibase s x y -> m (Dibase t x y))
    -> path
    -> s a -> m (t b)
pditransM = sditransM id
