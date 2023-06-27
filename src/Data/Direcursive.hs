{-|
Module      : Data.Direcursive
Description : Direcursion schemes and base bifunctors for recursive functors.
Copyright   : (c) Leo Dillinger
License     : MIT
Maintainer  : leo@apotheca.io
Stability   : experimental
Portability : POSIX

Direcursion schemes and base bifunctors for (right) recursive functors.

In addition to performing the same functions as mono-recursion schemes,
di-recursion schemes allow for mapping over the recursive functor's content
at the same time.

The prefix 'di-' is used here to mean 'half'. Whereas a recursive data type
has a base functor, a (right) direcursive functor only recurses in half of its
base bifunctor's slots - the right slot. Consequently, we may use the term
'difunctor' to refer to a recursive functor. All difunctors are direcursive,
and all direcursives are recursive functors.

The 'di-' functions can be nested to compose and transform recursive stacks.
This allows arbitrary access to inner layers, not unlike functor / monad transformer
stacks, and is especially useful for partial / distributed data structures.

The 'di-' functions can be used to derive the original fmap functions. For instance:

@
fmap = diiso
@

The 'di-' functions can be used to derive the original monorecursive functions. For example:

@
cata == dicata id
ana == diana id
hylo == dihylo id
@

The di- variants can also be derived from the mono- variants:

@
dihylo :: (forall x . Functor (f x), Bifunctor f) => (a -> b) -> (f b d -> d) -> (c -> f a c) -> c -> d
dihylo f alg coalg = hylo alg (first f . coalg)
@

However, making the functor explicit in the type-class avoids putting `forall` in all
of the function types.

NOTE: We could add a `forall a . Recursive (f a)` constraint to `Direcursive`
and `Codirecursive` , to enforce the relation to `Recursive` and `Corecursive`.
However, `forall x y . Dibase f x y ~ Base (f x) y` yields the error
'Illegal type synonym family application'. Since we cannot yet enforce that
`Dibase` ~ `Base`, we will not yet enforce the relation to `Recursive`.

-}

module Data.Direcursive
(
-- * Type classes
-- $typeClasses
  Dibase
, Direcursive(..)
, Codirecursive(..)
, Ditrans(..)
, Diiso(..)
, Difunctor(..)
-- * Basic difolds
-- $basicFolds
, dicata
, diana
, dihylo
-- * Applicative difolds
-- $applicativeFolds
, dicataA
, dianaA
, dihyloA
-- * Monadic difolds
-- $monadicFolds
, diprojectM
, diembedM
, dicataM
, dianaM
, dihyloM
-- * Special difolds
-- $specialFolds
, diiso
, ditrans
-- , diisoA
-- , ditransA
, diisoM
, ditransM
-- * Aliases
-- $aliases
, Base1
, Recursive1
, Corecursive1
, Iso1
, Trans1
, Fix1
, Free1
, Cofree1
, foldMap
, unfoldMap
, refoldMap
, refixMap
, hoistMap
-- * Free boolean cube
-- $freeBooleanCube
, Difix(..)
, Difree(..)
, Dicofree(..)
) where

import Data.Function
import Data.Functor
import Data.Foldable (Foldable)
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Control.Applicative
import Control.Monad

import Data.Recursive

-- $typeClasses

-- | The base bifunctor for recursive functors
type family Dibase (t :: * -> *) :: * -> * -> *

-- | The class of recursive functors that can be unfolded a single layer at a time.
-- TODO: Add: Functor f, forall a . Recursive (f a), forall a . Corecursive (f a)
class (Bifunctor (Dibase f)) => Direcursive f where
    -- | Unfold a single recursion layer.
    diproject :: f a -> Dibase f a (f a)

-- | The class of co-recursive functors that can be folded up a single layer at a time.
-- TODO: Add: Functor f, forall a . Recursive (f a), forall a . Corecursive (f a)
class (Bifunctor (Dibase f)) => Codirecursive f where
    -- | Fold up a single recursion layer.
    diembed :: Dibase f a (f a) -> f a

type Ditrans a b = (Direcursive a, Codirecursive b)

-- | The type constraint for isomorphic functors.
--
-- See `diiso`.
type Diiso a b = (Ditrans a b, Dibase a ~ Dibase b)

type Didis r = Diiso r r -- Ditrans r r -- (Direcursive r, Codirecursive r)

-- | The class of recursive functors
--
-- NOTE: We are aware of the pre-existing use of the terminology 'difunctor' as
-- an alias for 'profunctor' (hence why profunctor implements dimap). If we could,
-- we would rename 'Profunctor.dimap' to 'Profunctor.promap' and then claim 'dimap'
-- for this library to mean 'ditrans', which is a combination of fmap and trans.
class (Functor f, Direcursive f, Codirecursive f) => Difunctor f where
    -- | @dimap = ditrans@
    dimap :: (a -> b) -> (forall c . Dibase f b c -> Dibase g b c) -> f a -> g b
    -- | @lmap = fmap@
    lmap :: (a -> b) -> f a -> f b
    -- | @rmap = trans@
    rmap :: (forall b. Dibase f a b -> Dibase g a b) -> f a -> g a


-- $basicFolds
-- The di- variants of the basic folds

-- | A pure dicatamorphism
--
-- An alias for `foldMap` (not Data.Foldable.foldMap)
dicata :: (Direcursive f) => (a -> b) -> (Dibase f b c -> c) -> f a -> c
dicata f alg = dihylo f alg diproject

-- | A pure dianamorphism
--
-- An alias for `unfoldMap`
diana :: (Codirecursive f) => (a -> b) -> (c -> Dibase f a c) -> c -> f b
diana f coalg = dihylo f diembed coalg

-- | A pure dihylomorphism
--
-- An alias for `refoldMap`
dihylo :: (Bifunctor f) => (a -> b) -> (f b d -> d) -> (c -> f a c) -> c -> d
dihylo f alg coalg = h where h = alg . bimap f h . coalg

-- $applicativeFolds
-- Applicative variants of the basic difolds
-- 
-- In addition to pure and monadic variants, di- recursion schemes also allow
-- for applicative variants, unlike both mono- and bi- recursion schemes.
-- This is because neither mono- nor bi- recursion schemes are over functors.

-- Unused except as diprojectM
diprojectA :: (Applicative m, Direcursive f) => f a -> m (Dibase f a (f a))
diprojectA = pure . diproject

-- Unused except as diembedM
diembedA :: (Applicative m, Codirecursive f) => Dibase f a (f a) -> m (f a)
diembedA = pure . diembed

-- | An applicative variant of `dicata`
dicataA :: (Applicative f, Direcursive t, Bitraversable (Dibase t)) => (a -> f b) -> (Dibase t b c -> c) -> t a -> f c
dicataA f alg = dihyloA f alg diproject

-- | An applicative variant of `diana`
dianaA :: (Applicative f, Codirecursive t, Bitraversable (Dibase t)) => (a -> f b) -> (c -> Dibase t a c) -> c -> f (t b)
dianaA f coalg = dihyloA f diembed coalg

-- | An applicative variant of `dihylo`
--
-- NOTE: I am not sure that a variant with `(t c d -> f d)` and  `(b -> f (t a b))` can exist
-- unless it is a monad.
dihyloA :: (Applicative f, Bitraversable t) => (a -> f c) -> (t c d -> d) -> (b -> t a b) -> b -> f d
dihyloA f alg coalg = h where h = fmap alg . bitraverse f h . coalg

-- $monadicFolds
-- Monadic variants of the basic difolds

-- | A monadic variant of `diembed`
diprojectM :: (Monad m, Direcursive f) => f a -> m (Dibase f a (f a))
diprojectM = pure . diproject

-- | A monadic variant of `diproject`
diembedM :: (Monad m, Codirecursive f) => Dibase f a (f a) -> m (f a)
diembedM = pure . diembed

-- | A monadic variant of `dicata`
dicataM :: (Monad m, Direcursive f, Bitraversable (Dibase f)) => (a -> m b) -> (Dibase f b c -> m c) -> f a -> m c
dicataM f alg = dihyloM f alg diprojectM

-- | A monadic variant of `diana`
dianaM :: (Monad m, Codirecursive f, Bitraversable (Dibase f)) => (a -> m b) -> (c -> m (Dibase f a c)) -> c -> m (f b)
dianaM f coalg = dihyloM f diembedM coalg

-- | A monadic variant of `dihylo`
dihyloM :: (Monad m, Bitraversable f) => (a -> m b) -> (f b d -> m d) -> (c -> m (f a c)) -> c -> m d
dihyloM f alg coalg = h where h = alg <=< bitraverse f h <=< coalg

-- TODO: difooA_ and difooM_

-- $specialFolds

-- | The isomorphic map function
--
-- An alias for `refixMap`
--
-- We can derive `fmap` trivially by constraining `diiso` to use the same type in both slots,
-- and any `Diiso f f` is already a functor.
--
-- > diiso :: (Diiso s t) => (a -> b) -> s a -> t b
-- > fmap :: (Functor f) => (a -> b) -> f a -> t b
-- > fmap = diiso
diiso :: (Diiso s t) => (a -> b) -> s a -> t b
diiso f = dihylo f diembed diproject

-- | The natural transformation mapping function
--
-- An alias for `hoistMap`
ditrans :: (Ditrans s t) => (a -> b) -> (forall c . Dibase s b c -> Dibase t b c) -> s a -> t b
ditrans f g = dihylo f (diembed . g) diproject

diisoM :: (Monad m, Diiso s t, Bitraversable (Dibase s)) => (a -> m b) -> s a -> m (t b)
diisoM f = dihyloM f diembedM diprojectM

ditransM :: (Monad m, Ditrans s t, Bitraversable (Dibase s)) => (a -> m b) -> (forall c . Dibase s b c -> m (Dibase t b c)) -> s a -> m (t b)
ditransM f g = dihyloM f (diembedM <=< g) diprojectM

-- $aliases
-- Aliases to match the existing standard nomenclature.

type Base1 t        = Dibase t
type Recursive1 f   = Direcursive f
type Corecursive1 f = Codirecursive f
type Iso1 f g       = Diiso f g
type Trans1 f g     = Ditrans f g
type Fix1 f         = Difix f
type Free1 f a b    = Difree f a b
type Cofree1 f a b  = Dicofree f a b

-- | Folds a recursive functor into a value, one layer at a time, while mapping over the functor content.
--
-- An alias for `dicata`
foldMap :: (Recursive1 f) => (a -> b) -> (Base1 f b c -> c) -> f a -> c
foldMap = dicata

-- | Unfolds a value into a recursive type, one layer at a time, while mapping over the functor content.
--
-- An alias for `diana`
unfoldMap :: (Corecursive1 f) => (a -> b) -> (c -> Base1 f a c) -> c -> f b
unfoldMap = diana

-- | Unfolds a value into a recursive functor, maps over the functor content,
-- then (re)folds the result into another value.
--
-- An alias for `dihylo`
refoldMap :: (Bifunctor f) => (a -> b) -> (f b d -> d) -> (c -> f a c) -> c -> d
refoldMap = dihylo

-- | Convert from one recursive base representation to another, while mapping over the functor content.
--
-- An alias for `diiso`
refixMap :: (Iso1 s t) => (a -> b) -> s a -> t b
refixMap = diiso

-- | Convert from one recursive functor to another, while mapping over the functor content.
-- 
-- An alias for `ditrans`
hoistMap :: (Trans1 s t) => (a -> b) -> (forall c . Base1 s b c -> Base1 t b c) -> s a -> t b
hoistMap = ditrans

-- $freeBooleanCube
-- These data types are part of the free boolean cube, extended to di-recursion.

data Difix f a = Difix { unDifix :: (f a (Difix f a)) }

type instance Dibase (Difix f) = f

instance (Bifunctor f) => Functor (Difix f) where
    fmap = diiso

instance (Bifunctor f) => Direcursive (Difix f) where
    diproject = unDifix

instance (Bifunctor f) => Codirecursive (Difix f) where
    diembed = Difix

-- Or Difree f ann b
data Difree f a ann = Dipure ann | Difree (f a (Difree f a ann))
data Dicofree f a ann = Dicofree ann (f a (Dicofree f a ann))
