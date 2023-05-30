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

imapWith
    :: (IndexedFunctor f)
    => (Index f -> path -> indexpath)
    -> (indexpath -> a -> b)
    -> path -> f a -> f b
imapWith cons f p = imap (\ i -> f (cons i p))
    
class (Indexed f, Foldable f) => IndexedFoldable f where
    ifoldMap :: (Monoid m) => (Index f -> a -> m) -> f a -> m 
    ifoldMap = ifoldMapDefault

ifoldMapDefault :: (Indexed f, Foldable f, Monoid m) => (Index f -> a -> m) -> f a -> m
ifoldMapDefault f = foldMap (uncurry f) . indexed

-- TODO: ifoldMapWith
    
class (Indexed t, Traversable t) => IndexedTraversable t where
    itraverse :: Applicative f => (Index t -> a -> f b) -> t a -> f (t b) 
    itraverse = itraverseDefault

itraverseDefault :: (Indexed t, Traversable t, Applicative f) => (Index t -> a -> f b) -> t a -> f (t b) 
itraverseDefault f = traverse (uncurry f) . indexed

itraverseWith
    :: (IndexedTraversable t, Applicative f)
    => (Index t -> path -> indexpath)
    -> (indexpath -> a -> f b)
    -> path -> t a -> f (t b)
itraverseWith cons f p = itraverse (\ i -> f (cons i p))
