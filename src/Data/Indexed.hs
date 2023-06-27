{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Indexed where

import Data.Function
import Data.Functor
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Data.Tuple

import Control.Applicative

type family Index (f :: * -> *) :: *

class Indexed f where
    indexed :: f a -> f (Index f, a)
    default indexed :: (IndexedFunctor f) => f a -> f (Index f, a)
    indexed = indexedDefault

indexedDefault :: (IndexedFunctor f) => f a -> f (Index f, a)
indexedDefault = imap (,)
    
class (Indexed f, Functor f) => IndexedFunctor f where
    imap :: (Index f -> a -> b) -> f a -> f b
    imap = imapDefault

imapDefault :: (Indexed f, Functor f) => (Index f -> a -> b) -> f a -> f b
imapDefault f = fmap (uncurry f) . indexed
    
class (Indexed f, Foldable f) => IndexedFoldable f where
    ifoldMap :: (Monoid m) => (Index f -> a -> m) -> f a -> m 
    ifoldMap = ifoldMapDefault

ifoldMapDefault :: (Indexed f, Foldable f, Monoid m) => (Index f -> a -> m) -> f a -> m
ifoldMapDefault f = foldMap (uncurry f) . indexed

-- TODO: pfoldMap et al

-- TODO: sfoldMap et al

-- TODO: This may be a more desirable hierarchy, despite Indexed being sufficient over IndexedFunctor + IndexedFoldable
-- class (IndexedFunctor t, IndexedFoldable t, Traversable t) => IndexedTraversable t where
class (Indexed t, Traversable t) => IndexedTraversable t where
    itraverse :: Applicative f => (Index t -> a -> f b) -> t a -> f (t b) 
    itraverse = itraverseDefault

itraverseDefault :: (Indexed t, Traversable t, Applicative f) => (Index t -> a -> f b) -> t a -> f (t b) 
itraverseDefault f = traverse (uncurry f) . indexed

-- NOTE: Defaults like:
-- fmap (uncurry f) . indexed
-- foldMap (uncurry f) . indexed
-- traverse (uncurry f) . indexed
-- Are only stricly equivalent to a properly implemented imap / ifoldMap / itraverse
-- for the outer perspective of a non-recursive structure.
-- For a recursive functor using folds, we want to operate over /the base functor/ and map in steps.
-- Mapping over every object and *then* doing something with the container's structure is different
-- from doing them seamlessly - remember that recursive objects embody both functor and fold,
-- functor for mapping, and fold for order of operations on children.
-- Operations need to be woven properly to satisfy the higher recursive functor constraint,
-- so `imap` for a recursive functor has the additional constraint of both self and children being
-- mapped / transformed in a specific order
