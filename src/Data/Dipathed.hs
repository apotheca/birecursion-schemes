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

import Control.Applicative
import Control.Monad

type Diindex (r :: * -> *) = (FirstIndex (Dibase r), [SecondIndex (Dibase r)]) -- NOTE: Should be ~ Index (r a)
type Dipath (r :: * -> *) = [SecondIndex (Dibase r)]                           -- NOTE: Should be ~ Path (r a)

-- NOTE: These constraints often end up adding little, due to incomplete hierarchy
--  Eg, `Direcursive t, BiindexedBitraversable (Dibase t)` is sufficient as compared
--  to `PathedDirecursive t, Bitraversable (Dibase t)` which isn't.
type DipathedDirecursive t = (Direcursive t, BiindexedBifunctor (Dibase t))
type DipathedCodirecursive t = (Codirecursive t, BiindexedBifunctor (Dibase t))

-- NOTE: We've dispensed with the i-variants for now

-- NOTE: Enforcing Functor, Recursive => Direcursive would allow for
--  enforcing (Index f ~ Diindex f, Path (f a) ~ Dipath f) implicitly
-- pdicata
--     :: (DipathedDirecursive f, Index f ~ Diindex f, Path (f a) ~ Dipath f)
--     => (Index f -> a -> b)
--     -> (Path (f a) -> Dibase f b d -> d)
--     -> Path (f a) -> f a -> d
pdicata
    :: (DipathedDirecursive f)
    => (Diindex f -> a -> b)
    -> (Dipath f -> Dibase f b d -> d)
    -> Dipath f -> f a -> d
pdicata f alg  = pdihylo f alg (const diproject)

pdiana
    :: (DipathedCodirecursive f)
    => (Diindex f -> a -> b)
    -> (Dipath f -> c -> Dibase f a c)
    -> Dipath f -> c -> f b
pdiana f coalg  = pdihylo f (const diembed) coalg

pdihylo 
    :: BiindexedBifunctor f
    => ((FirstIndex f, [SecondIndex f]) -> a -> b)
    -> ([SecondIndex f] -> f b d -> d)
    -> ([SecondIndex f] -> c -> f a c)
    -> [SecondIndex f] -> c -> d
-- pdihylo f alg coalg = h where h p = alg p . ibimap (\ i -> f (i , p)) (\ j -> h (j : p)) . coalg p
pdihylo f alg coalg = h where h p = alg p . ibimapWith (,) (:) f h p . coalg p

pdicataA
    :: (Applicative f, DipathedDirecursive t, BiindexedBitraversable (Dibase t))
    => (Diindex t -> a -> f b)
    -> (Dipath t -> Dibase t b c -> c)
    -> Dipath t -> t a -> f c
pdicataA f alg = pdihyloA f alg (const diproject)

pdianaA
    :: (Applicative f, DipathedCodirecursive t, BiindexedBitraversable (Dibase t))
    => (Diindex t -> a -> f b)
    -> (Dipath t -> c -> Dibase t a c)
    -> Dipath t -> c -> f (t b)
pdianaA f coalg = pdihyloA f (const diembed) coalg

pdihyloA
    :: (Applicative f, BiindexedBitraversable t)
    => ((FirstIndex t, [SecondIndex t]) -> a -> f b)
    -> ([SecondIndex t] -> t b d -> d)
    -> ([SecondIndex t] -> c -> t a c)
    -> [SecondIndex t] -> c -> f d
pdihyloA f alg coalg = h where
    h p = fmap (alg p) . ibitraverse (\ i -> f (i,p)) (\ j -> h (j : p)) . coalg p

pdicataM
    :: (Monad m, DipathedDirecursive f, BiindexedBitraversable (Dibase f))
    => (Diindex f -> a -> m b)
    -> (Dipath f -> Dibase f b d -> m d)
    -> Dipath f -> f a -> m d
pdicataM f alg  = pdihyloM f alg (const diprojectM)

pdianaM 
    :: (Monad m, DipathedCodirecursive f, BiindexedBitraversable (Dibase f))
    => (Diindex f -> a -> m b)
    -> (Dipath f -> c -> m (Dibase f a c))
    -> Dipath f -> c -> m (f b)
pdianaM f coalg  = pdihyloM f (const diembedM) coalg

pdihyloM
    :: (Monad m, BiindexedBitraversable f)
    => ((FirstIndex f, [SecondIndex f]) -> a -> m b)
    -> ([SecondIndex f] -> f b d -> m d)
    -> ([SecondIndex f] -> c -> m (f a c))
    -> [SecondIndex f] -> c -> m d
pdihyloM f alg coalg = h where
    -- h p = alg p <=< ibitraverse (\ i -> f (i,p)) (\ j -> h (j : p)) <=< coalg p
    h p = alg p <=< ibitraverseWith (,) (:) f h p <=< coalg p

pdiiso
    :: (Diiso s t, BiindexedBifunctor (Dibase s))
    => (Diindex s -> a -> b)
    -> Dipath s -> s a -> t b
pdiiso f = pdihylo f (const diembed) (const diproject)

pditrans
    :: (DipathedDirecursive s, Codirecursive t)
    => (Diindex s -> a -> b)
    -> (forall c . Dipath s -> Dibase s b c -> Dibase t b c)
    -> Dipath s -> s a -> t b
pditrans f g = pdihylo f (\ p -> diembed . g p) (const diproject)

pdiisoA
    :: (Applicative f, Diiso s t, BiindexedBitraversable (Dibase s))
    => (Diindex s -> a -> f b)
    -> Dipath s -> s a -> f (t b)
pdiisoA f = pdihyloA f (const diembed) (const diproject)

pditransA
    :: (Applicative f, DipathedDirecursive s, Codirecursive t, BiindexedBitraversable (Dibase s))
    => (Diindex s -> a -> f b)
    -> (forall c . Dipath s -> Dibase s b c -> Dibase t b c)
    -> Dipath s -> s a -> f (t b)
pditransA f g = pdihyloA f (\ p -> diembed . g p) (const diproject)

pdiisoM
    :: (Monad m, Diiso s t, BiindexedBitraversable (Dibase s))
    => (Diindex s -> a -> m b)
    -> Dipath s -> s a -> m (t b)
pdiisoM f = pdihyloM f (const diembedM) (const diprojectM)

pditransM
    :: (Monad m, DipathedDirecursive s, Codirecursive t, BiindexedBitraversable (Dibase s))
    => (Diindex s -> a -> m b)
    -> (forall c . Dipath s -> Dibase s b c -> m (Dibase t b c))
    -> Dipath s -> s a -> m (t b)
pditransM f g = pdihyloM f (\ p -> diembedM <=< g p) (const diprojectM)
