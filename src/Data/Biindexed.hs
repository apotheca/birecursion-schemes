module Data.Biindexed where

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Either
import Data.Function
import Data.Functor
import Data.Foldable
import Data.Indexed
import Data.Monoid
import Data.Traversable
import Data.Tuple

import Control.Applicative
import Control.Monad

-- type family FirstIndex (f :: * -> * -> *) :: *
-- type family SecondIndex (f :: * -> * -> *) :: *

-- type Biindex (f :: * -> * -> *) = Either (FirstIndex f) (SecondIndex f)

type family FirstIndex (f :: * -> * -> *) :: *
type family SecondIndex (f :: * -> * -> *) :: *

type Biindex (f :: * -> * -> *) = Either (FirstIndex f) (SecondIndex f)

class Biindexed f where
    biindexed :: f a b -> f (FirstIndex f, a) (SecondIndex f, b)
    default biindexed :: (BiindexedBifunctor f) => f a b -> f (FirstIndex f, a) (SecondIndex f, b)
    biindexed = biindexedDefault

biindexedDefault :: (BiindexedBifunctor f) => f a b -> f (FirstIndex f, a) (SecondIndex f, b)
biindexedDefault = ibimap (,) (,)
    
class (Biindexed f, Bifunctor f) => BiindexedBifunctor f where
    ibimap :: (FirstIndex f -> a -> b) -> (SecondIndex f -> c -> d) -> f a c -> f b d
    ibimap = ibimapDefault

ibimapDefault :: (Biindexed f, Bifunctor f) => (FirstIndex f -> a -> b) -> (SecondIndex f -> c -> d) -> f a c -> f b d
ibimapDefault f g = bimap (uncurry f) (uncurry g) . biindexed

class (Biindexed f, Bifoldable f) => BiindexedBifoldable f where
    ibifoldMap :: (Monoid m) => (FirstIndex f -> a -> m) -> (SecondIndex f -> b -> m) -> f a b -> m
    ibifoldMap = ibifoldMapDefault

ibifoldMapDefault :: (Biindexed f, Bifoldable f, Monoid m) => (FirstIndex f -> a -> m) -> (SecondIndex f -> b -> m) -> f a b -> m
ibifoldMapDefault f g = bifoldMap (uncurry f) (uncurry g) . biindexed
    
class (Biindexed t, Bitraversable t) => BiindexedBitraversable t where
    ibitraverse :: Applicative f => (FirstIndex t -> a -> f c) -> (SecondIndex t -> b -> f d) -> t a b -> f (t c d)
    ibitraverse = ibitraverseDefault

ibitraverseDefault :: (Biindexed t, Bitraversable t, Applicative f) => (FirstIndex t -> a -> f c) -> (SecondIndex t -> b -> f d) -> t a b -> f (t c d)
ibitraverseDefault f g = bitraverse (uncurry f) (uncurry g) . biindexed
