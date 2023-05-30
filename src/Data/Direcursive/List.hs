module Data.Direcursive.List where

import Data.Function
import Data.Functor
import Data.Foldable
import Data.Bifunctor
import Data.Bifoldable

import Prelude

import Data.Recursive
import Data.Direcursive

data List a = Nil | Cons a (List a)

type instance Base (List a) = ListF a
type instance Dibase List = ListF

instance Recursive (List a) where
    project = diproject

instance Corecursive (List a) where
    embed = diembed

instance Direcursive List where
    diproject Nil = NilF
    diproject (Cons a r) = ConsF a r

instance Codirecursive List where
    diembed NilF = Nil
    diembed (ConsF a r) = Cons a r

instance Functor List where
    fmap = diiso

data ListF a r = NilF | ConsF a r

instance Functor (ListF a) where
    fmap = second

instance Bifunctor ListF where
    bimap _ _ NilF = NilF
    bimap f g (ConsF a r) = ConsF (f a) (g r)

-- NOTE: Can we derive:
-- instance Foldable List where
--     -- foldr :: (a -> b -> b) -> b -> List a -> b
--     foldr f z Nil = z
--     foldr f z (Cons a r) = f a (foldr f z r)
-- from:
-- instance Bifoldable ListF where
--     -- bifoldr :: (a -> c -> c) -> (b -> c -> c) -> c -> ListF a b -> c
--     bifoldr f g z NilF = z
--     bifoldr f g z (ConsF a r) = f a $ g r z
