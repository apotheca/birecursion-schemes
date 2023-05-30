{-|
Module      : Data.Birecursive
Description : Birecursion schemes and base bifunctors for birecursive data types.
Copyright   : (c) Leo Dillinger
License     : MIT
Maintainer  : leo@apotheca.io
Stability   : experimental
Portability : POSIX

Birecursion schemes and base bifunctors for birecursive data types.

These functions are less-explored than the di-recursive variants,
and their use case is less clear, though there is probable utility
in joining the functor content type and the recursive type into a
single unified type.
-}

module Data.Birecursive
(
-- * Type classes
-- $typeClasses
  Bibase
, Birecursive(biproject)
, Cobirecursive(biembed)
, Biiso(..)
-- * Basic bifolds
-- $basicFolds
, bicata
, biana
, bihylo
-- * Monadic bifolds
-- $monadicFolds
, biprojectM
, biembedM
, bicataM
, bianaM
, bihyloM
-- * Special bifolds
-- $specialFolds
, biiso
, bitrans
, biisoM
, bitransM
-- * Aliases
-- $aliases
, bifold
, biunfold
, birefold
, birefix
, bihoist
-- * Free Boolean Cube
-- $freeBooleanCube
, Bifix(..)
, Bifree(..)
, Bicofree(..)
) where

import Data.Function
import Data.Functor
import Data.Foldable
import Data.Bifunctor
import Data.Bifoldable hiding (bifold)
import Data.Bitraversable
import Control.Applicative
import Control.Monad

-- | The base bifunctor for a bi-recursive data type
type family Bibase a :: (* -> * -> *)

-- | The class of bi-recursive data types that can be unfolded a single layer at a time.
class (Bifunctor (Bibase t)) => Birecursive t where
    -- | Unfold a single recursion layer.
    biproject :: t -> Bibase t t t

-- | The class of co-bi-recursive data types that can be folded up a single layer at a time.
class (Bifunctor (Bibase t)) => Cobirecursive t where
    -- | Fold up a single recursion layer.
    biembed :: Bibase t t t -> t

-- | The type constraint for bi- isomorphic data types.
--
-- See `biiso`.
type Biiso a b = (Birecursive a, Cobirecursive b, Bibase a ~ Bibase b)

-- $basicFolds
-- The bi- variants of the basic folds

-- | A pure bicatamorphism
--
-- An alias for `bifold` (not Data.Bifoldable.bifold)
bicata :: (Birecursive a) => (Bibase a b b -> b) -> a -> b
bicata alg = bihylo alg biproject

-- | A pure bianamorphism
--
-- An alias for `biunfold`
biana :: (Cobirecursive b) => (a -> Bibase b a a) -> a -> b
biana = bihylo biembed

-- | A pure bihylomorphism
--
-- An alias for `birefold`
bihylo :: (Bifunctor f) => (f b b -> b) -> (a -> f a a) -> a -> b
bihylo alg coalg = h where h = alg . bimap h h . coalg

-- NOTE: There are no applicative variants for recursive

-- $monadicFolds
-- Monadic variants of the basic bifolds

-- | A monadic variant of `biembed`
biprojectM :: (Monad m, Birecursive t) => t -> m (Bibase t t t)
biprojectM = pure .  biproject
-- | A monadic variant of `biproject`
biembedM :: (Monad m, Cobirecursive t) => Bibase t t t -> m t
biembedM = pure .  biembed
-- | A monadic variant of `bicata`
bicataM :: (Monad m, Bitraversable (Bibase a), Birecursive a) => (Bibase a b b -> m b) -> a -> m b
bicataM alg = bihyloM alg biprojectM
-- | A monadic variant of `biana`
bianaM :: (Monad m, Bitraversable (Bibase b), Cobirecursive b) => (a -> m (Bibase b a a)) -> a -> m b
bianaM = bihyloM biembedM
-- | A monadic variant of `bihylo`
bihyloM :: (Monad m, Bifunctor f, Bitraversable f) => (f b b -> m b) -> (a -> m (f a a)) -> a -> m b
-- bihyloM alg coalg = h where h = alg <=< bimapM h h <=< coalg
bihyloM alg coalg = h where h = alg <=< bitraverse h h <=< coalg

-- $specialFolds

-- | The bi- isomorpism function
--
-- An alias for `birefix`
biiso :: (Biiso a b) => a -> b
biiso = bihylo biembed biproject

-- | The bi- natural transformation function / natural transformation of bifunctors
--
-- An alias for `bihoist`
bitrans :: (Birecursive s, Cobirecursive t) => (forall a b. Bibase s a b -> Bibase t a b) -> s -> t
bitrans f = bihylo (biembed . f) biproject

-- | The natural bi-transformation of bifunctors
-- bibitrans :: (Cobirecursive b, Birecursive a, Bifunctor f) => (f b b -> Bibase b b b) -> (Bibase a a a -> f a a) -> a -> b
-- bibitrans f g = bihylo (biembed . f) (g . biproject)

-- otherbibitrans :: (Bibase c ~ Bibase b, Cobirecursive b, Birecursive c) => (b -> b) -> (c -> c) -> c -> b
-- otherbibitrans f g = h where h = biembed . bimap (f . h) (h . g) . biproject

-- Barely non-trivial
biisoM :: (Monad m, Biiso a b, Bitraversable (Bibase a)) => a -> m b
biisoM = bihyloM biembedM biprojectM

-- Non-trivial
bitransM :: (Monad m, Birecursive s, Cobirecursive t, Bitraversable (Bibase s)) => (forall a b. Bibase s a b -> m (Bibase t a b)) -> s -> m t
bitransM f = bihyloM (biembedM <=< f) biprojectM

-- $aliases
-- Aliases to match the existing standard nomenclature.

-- | Folds a birecursive type into a value, one layer at a time.
--
-- An alias for `bicata`
bifold :: (Birecursive a) => (Bibase a b b -> b) -> a -> b
bifold = bicata

-- | Unfolds a value into a birecursive type, one layer at a time.
--
-- An alias for `biana`
biunfold :: (Cobirecursive b) => (a -> Bibase b a a) -> a -> b
biunfold = biana

-- | Unfolds a value into a birecursive type, then (re)folds that type into another value.
--
-- An alias for `bihylo`
birefold :: (Bifunctor f) => (f b b -> b) -> (a -> f a a) -> a -> b
birefold = bihylo

-- | Convert from one birecursive representation to another.
--
-- An alias for `biiso`
birefix :: (Biiso a b) => a -> b
birefix = biiso

-- | Convert from one birecursive type to another.
-- 
-- An alias for `bitrans`
bihoist :: (Birecursive s, Cobirecursive t) => (forall a b. Bibase s a b -> Bibase t a b) -> s -> t
bihoist = bitrans

-- $freeBooleanCube
-- These data types are part of the free boolean cube, extended to bi-recursion.

data Bifix t = Bifix { unBifix :: t (Bifix t) (Bifix t) }

type instance Bibase (Bifix f) = f

instance (Bifunctor f) => Birecursive (Bifix f) where
    biproject = unBifix

instance (Bifunctor f) => Cobirecursive (Bifix f) where
    biembed = Bifix

data Bifree f ann = Bipure ann | Bifree (f (Bifree f ann) (Bifree f ann))
data Bicofree f ann = Bicofree ann (f (Bicofree f ann) (Bicofree f ann))