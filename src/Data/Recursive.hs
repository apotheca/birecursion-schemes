{-# LANGUAGE CPP #-}
{-|
Module      : Data.Recursive
Description : Monorecursion schemes and base functors for recursive data types.
Copyright   : (c) Leo Dillinger
License     : MIT
Maintainer  : leo@apotheca.io
Stability   : experimental
Portability : POSIX

Monorecursion schemes and base functors for recursive data types.

The term 'mono' here is used here in the same manner as `MonoFunctor`,
because a mono-recursive data type is a mono-functor where the element
is of the same type as itself.
-}

module Data.Recursive
(
-- * Type classes
-- $typeClasses
  Base
, Recursive(project)
, Corecursive(embed)
, Iso(..)
-- * Basic folds
-- $basicFolds
, cata
, ana
, hylo
-- * Monadic folds
-- $monadicFolds
, projectM
, embedM
, cataM
, anaM
, hyloM
-- * Special folds
-- $specialFolds
, iso
, trans
-- * Aliases
-- $aliases
, fold
, unfold
, refold
, refix
, hoist
-- * Free Boolean Cube
-- $freeBooleanCube
, Fix(..)
, Free(..)
, Cofree(..)
) where

import Data.Foldable (Foldable)
import Data.Function
import Data.Functor
import Data.Traversable
import Control.Applicative
import Control.Monad


-- $typeClasses
-- Type classes, families, and aliases that are necessary for this module.

#ifdef NO_RECURSION_SCHEMES
-- Don't import Data.Functor.Foldable
#else
import Data.Functor.Foldable (Base(..), Recursive(project), Corecursive(embed))
#endif

#ifdef NO_RECURSION_SCHEMES

-- | The base functor for a mono-recursive data type
type family Base t :: * -> *

-- | The class of mono-recursive data types that can be unfolded a single layer at a time.
class Functor (Base t) => Recursive t where
    -- | Unfold a single recursion layer.
    project :: t -> Base t t

-- | The class of co-mono-recursive data types that can be folded up a single layer at a time.
class Functor (Base t) => Corecursive t where
    -- | Fold up a single recursion layer.
    embed :: Base t t -> t

#else
-- Don't define Base, Recursive, Corecursive
#endif

-- | The type constraint for isomorphic data types.
--
-- See `iso`.
type Iso a b = (Recursive a, Corecursive b, Base a ~ Base b)

-- class Recursive t => Indexed t where

-- $basicFolds
-- The basic folds that this library focuses on

-- | A pure catamorphism
--
-- An alias for `fold`
cata :: (Recursive t) => (Base t a -> a) -> t -> a
cata alg = hylo alg project

-- | A pure anamorphism
--
-- An alias for `unfold`
ana :: (Corecursive t) => (a -> Base t a) -> a -> t
ana = hylo embed

-- | A pure hylomorphism
--
-- An alias for `refold`
hylo :: (Functor f) => (f b -> b) -> (a -> f a) -> a -> b
hylo alg coalg = h where h = alg . fmap h . coalg

-- NOTE: There are no applicative variants for recursive

-- $monadicFolds
-- Monadic variants of the basic folds

-- | A monadic variant of `embed`
embedM :: (Monad m, Corecursive t) => Base t t -> m t
embedM = return . embed

-- | A monadic variant of `project`
projectM :: (Monad m, Recursive t) => t -> m (Base t t)
projectM = return . project

-- | A monadic variant of `cata`
cataM :: (Monad m, Recursive t, Traversable f, f ~ Base t) => (f a -> m a) -> t -> m a
cataM alg = hyloM alg projectM

-- | A monadic variant of `ana`
anaM :: (Monad m, Corecursive t, Traversable f, f ~ Base t) => (a -> m (f a)) -> a -> m t
anaM = hyloM embedM

-- | A monadic variant of `hylo`
hyloM :: (Monad m, Traversable t) => (t b -> m b) -> (a -> m (t a)) -> a -> m b
hyloM alg coalg = h where h = alg <=< traverse h <=< coalg

-- $specialFolds

-- | The isomorpism function
--
-- An alias for `refix`
iso :: (Iso s t) => s -> t
iso = hylo embed project

-- | The natural transformation function
--
-- An alias for `hoist`
trans :: (Recursive s, Corecursive t) => (forall a. Base s a -> Base t a) -> s -> t
trans f = hylo (embed . f) project

-- $aliases
-- Aliases to match the existing standard nomenclature.

-- | Folds a recursive type into a value, one layer at a time.
--
-- An alias for `cata`
fold :: (Recursive t) => (Base t a -> a) -> t -> a
fold = cata

-- | Unfolds a value into a recursive type, one layer at a time.
--
-- An alias for `ana`
unfold :: (Corecursive t) => (a -> Base t a) -> a -> t
unfold = ana

-- | Unfolds a value into a recursive type, then (re)folds that type into another value.
--
-- An alias for `hylo`
refold :: (Functor f) => (f b -> b) -> (a -> f a) -> a -> b
refold = hylo

-- | Convert from one recursive representation to another.
--
-- An alias for `iso`
refix :: (Iso s t) => s -> t
refix = iso

-- | Convert from one recursive type to another.
-- 
-- An alias for `trans`.
hoist :: (Recursive s, Corecursive t) => (forall a. Base s a -> Base t a) -> s -> t
hoist = trans

-- $freeBooleanCube
-- These data types are part of the free boolean cube

data Fix f = Fix { unFix :: (f (Fix f)) }

type instance Base (Fix f) = f

instance (Functor f) => Recursive (Fix f) where
    project = unFix
    
instance (Functor f) => Corecursive (Fix f) where
    embed = Fix

data Free f ann = Pure ann | Free (f (Free f ann))
data Cofree f ann = Cofree ann (f (Cofree f ann))
