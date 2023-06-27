module Data.Sourced where


import Data.Bifunctor
import Data.Bifoldable
import Data.Biindexed
import Data.Bitraversable
import Data.Birecursive
import Data.Direcursive
import Data.Foldable (Foldable)
import Data.Function
import Data.Functor
import Data.Indexed
import Data.Recursive
import Data.Traversable

import Control.Applicative
import Control.Monad

-- TODO: We could derive all of the p- morphisms using smap etc, and
--  then derive the s- morphisms this way. Does it matter?
{-
phylo'
    :: (IndexedFunctor f)
    => (Index f -> path -> path)
    -> (path -> f b -> b)
    -> (path -> a -> f a)
    -> path
    -> a -> b
phylo' cons alg coalg = h where
    h p = alg p . smap cons h p . coalg p

shylo'
    :: (IndexedFunctor f)
    => (source -> path)             -- End
    -> (Index f -> path -> path)    -- Cons
    -> (path -> f b -> b)
    -> (path -> a -> f a)
    -> source                       -- End's argument
    -> a -> b
shylo' end cons alg coalg = phylo' cons alg coalg . end
-}

--
-- Sourced maps
--

smap
    :: (IndexedFunctor f)
    => (Index f -> source -> index)     -- Cons
    -> (index -> a -> b)
    -> source                           -- Nil
    -> f a -> f b
smap cons f src = imap (\ i -> f (cons i src))

straverse
    :: (IndexedTraversable t, Applicative f)
    => (Index t -> source -> index)
    -> (index -> a -> f b)
    -> source -> t a -> f (t b)
straverse cons f src = itraverse (\ i -> f (cons i src))

sbimap
    :: (BiindexedBifunctor f)
    => (FirstIndex f -> source -> findex)     -- Left Cons
    -> (SecondIndex f -> source -> sindex)    -- Right Cons
    -> (findex -> a -> b)
    -> (sindex -> c -> d)
    -> source -> f a c -> f b d
sbimap icons jcons f g p = ibimap (\ i -> f (icons i p)) (\ j -> g (jcons j p))

sbitraverse
    :: (Applicative f, BiindexedBitraversable t)
    => (FirstIndex t -> source -> findex)     -- Left Cons
    -> (SecondIndex t -> source -> sindex)    -- Right Cons
    -> (findex -> a -> f b)
    -> (sindex -> c -> f d)
    -> source -> t a c -> f (t b d)
sbitraverse icons jcons f g p = ibitraverse (\ i -> f (icons i p)) (\ j -> g (jcons j p))

--
-- Sourced morphisms
--

scata
    :: (IndexedFunctor (Base a), Recursive a)
    => (source -> path)
    -> (Index (Base a) -> path -> path)
    -> (path -> Base a b -> b)
    -> source
    -> a -> b
scata end cons alg = shylo end cons alg (const project)

sana
    :: (IndexedFunctor (Base b), Corecursive b)
    => (source -> path)
    -> (Index (Base b) -> path -> path)
    -> (path -> a -> Base b a)
    -> source
    -> a -> b
sana end cons coalg = shylo end cons (const embed) coalg

shylo
    :: (IndexedFunctor f)
    => (source -> path)             -- End
    -> (Index f -> path -> path)    -- Cons
    -> (path -> f b -> b)
    -> (path -> a -> f a)
    -> source                       -- End's argument
    -> a -> b
shylo end cons alg coalg = h . end where
    h p = alg p . smap cons h p . coalg p

-- Trivial, siso _ _ _ = iso
siso
    :: (IndexedFunctor (Base a), Iso a b)
    => (source -> path)
    -> (Index (Base a) -> path -> path)
    -> source
    -> a -> b
siso end cons = shylo end cons (const embed) (const project)

-- NOTE: Inferring the type *will forget* the forall
strans
    :: (IndexedFunctor (Base s), Trans s t)
    => (source -> path)
    -> (Index (Base s) -> path -> path)
    -> (path -> forall a . Base s a -> Base t a) -- Should this forall be before 'path ->'?
    -> source
    -> s -> t
strans end cons f = shylo end cons (\ p -> embed . f p) (const project)

--
-- Sourced bimorphisms
--

sbicata
    :: (Birecursive a, BiindexedBifunctor (Bibase a)) 
    => (source -> path)
    -> (FirstIndex (Bibase a) -> path -> path)
    -> (SecondIndex (Bibase a) -> path -> path)
    -> (path -> Bibase a b b -> b)
    -> source
    -> a -> b
sbicata end icons jcons alg = sbihylo end icons jcons alg (const biproject)

sbiana
    :: (Cobirecursive b, BiindexedBifunctor (Bibase b)) 
    => (source -> path)
    -> (FirstIndex (Bibase b) -> path -> path)
    -> (SecondIndex (Bibase b) -> path -> path)
    -> (path -> a -> Bibase b a a)
    -> source
    -> a -> b
sbiana end icons jcons coalg = sbihylo end icons jcons (const biembed) coalg

sbihylo
    :: (BiindexedBifunctor f)
    => (source -> path)                 -- End
    -> (FirstIndex f -> path -> path)   -- Left cons
    -> (SecondIndex f -> path -> path)  -- Right cons
    -> (path -> f b b -> b)
    -> (path -> a -> f a a)
    -> source                           -- End's argument
    -> a -> b
sbihylo end icons jcons alg coalg = h . end where
    h p = alg p . sbimap icons jcons h h p . coalg p

-- Trivial
sbiiso
    :: (Biiso a b, BiindexedBifunctor (Bibase a))
    => (source -> path)
    -> (FirstIndex (Bibase b) -> path -> path)
    -> (SecondIndex (Bibase b) -> path -> path)
    -> source
    -> a -> b
sbiiso end icons jcons = sbihylo end icons jcons (const biembed) (const biproject)

sbitrans
    :: (Bitrans s t, BiindexedBifunctor (Bibase s)) 
    => (source -> path)
    -> (FirstIndex (Bibase s) -> path -> path)
    -> (SecondIndex (Bibase s) -> path -> path)
    -> (path -> forall a b . Bibase s a b -> Bibase t a b)
    -> source
    -> s -> t
sbitrans end icons jcons f = sbihylo end icons jcons (\ p -> biembed . f p) (const biproject)

--
-- Sourced dimorphisms
--

sdicata
    :: (BiindexedBifunctor (Dibase f), Direcursive f)
    => (source -> path)
    -> (FirstIndex (Dibase f) -> path -> index)
    -> (SecondIndex (Dibase f) -> path -> path)
    -> (index -> a -> b)
    -> (path -> Dibase f b d -> d)
    -> source
    -> f a -> d
sdicata end cap cons f alg  = sdihylo end cap cons f alg (const diproject)

sdiana
    :: (BiindexedBifunctor (Dibase f), Codirecursive f)
    => (source -> path)
    -> (FirstIndex (Dibase f) -> path -> index)
    -> (SecondIndex (Dibase f) -> path -> path)
    -> (index -> a -> b)
    -> (path -> c -> Dibase f a c)
    -> source
    -> c -> f b
sdiana end cap cons f coalg  = sdihylo end cap cons f (const diembed) coalg

sdihylo
    :: (BiindexedBifunctor f)
    => (source -> path)                 -- End
    -> (FirstIndex f -> path -> index)  -- Cap
    -> (SecondIndex f -> path -> path)  -- Cons
    -> (index -> a -> b)
    -> (path -> f b d -> d)
    -> (path -> c -> f a c)
    -> source                           -- End's argument
    -> c -> d
sdihylo end cap cons f alg coalg = h . end where
    h p = alg p . sbimap cap cons f h p . coalg p

sdiiso
    :: (Diiso s t, BiindexedBifunctor (Dibase s))
    => (source -> path)
    -> (FirstIndex (Dibase s) -> path -> index)
    -> (SecondIndex (Dibase s) -> path -> path)
    -> (index -> a -> b)
    -> source
    -> s a -> t b
sdiiso end cap cons f = sdihylo end cap cons f (const diembed) (const diproject)

sditrans
    :: (Ditrans s t, BiindexedBifunctor (Dibase s))
    => (source -> path)
    -> (FirstIndex (Dibase s) -> path -> index)
    -> (SecondIndex (Dibase s) -> path -> path)
    -> (index -> a -> b)
    -> (path -> forall x y .  Dibase s x y -> Dibase t x y)
    -> source
    -> s a -> t b
sditrans end cap cons f g = sdihylo end cap cons f (\ p -> diembed . g p) (const diproject)

--
-- Applicative sourced dimorphisms
--

-- Trivial, f is only used in the result
-- shyloA 
--     :: (IndexedTraversable t, Applicative f)
--     => (source -> path)
--     -> (Index t -> path -> path)
--     -> (path -> t b -> b)
--     -> (path -> a -> t a)
--     -> source
--     -> a -> f b
-- shyloA end cons alg coalg = h . end where
--     h p = fmap (alg p) . straverse cons h p . coalg p

-- Trivial, f is only used in the result
-- NOTE: Inferring the type *will forget* the forall
-- stransA
--     :: (Applicative f, IndexedTraversable (Base s), Recursive s, Corecursive t)
--     => (source -> path)
--     -> (Index (Base s) -> path -> path)
--     -> (path -> forall a . Base s a -> Base t a) -- Should this forall be before 'path ->'?
--     -> source
--     -> s -> f t
-- stransA end cons f = shyloA end cons (\ p -> embed . f p) (const project)

sdicataA
    :: (Applicative f, Direcursive t, BiindexedBitraversable (Dibase t))
    => (source -> path)                          -- End
    -> (FirstIndex (Dibase t) -> path -> index)  -- Cap
    -> (SecondIndex (Dibase t) -> path -> path)  -- Cons
    -> (index -> a -> f b)
    -> (path -> Dibase t b c -> c)
    -> source                                    -- End's argument
    -> t a -> f c
sdicataA end cap cons f alg = sdihyloA end cap cons f alg (const diproject)

sdianaA
    :: (Applicative f, Codirecursive t, BiindexedBitraversable (Dibase t))
    => (source -> path)                          -- End
    -> (FirstIndex (Dibase t) -> path -> index)  -- Cap
    -> (SecondIndex (Dibase t) -> path -> path)  -- Cons
    -> (index -> a -> f b)
    -> (path -> c -> Dibase t a c)
    -> source                                    -- End's argument
    -> c -> f (t b)
sdianaA end cap cons f coalg = sdihyloA end cap cons f (const diembed) coalg

sdihyloA
    :: (Applicative f, BiindexedBitraversable t)
    => (source -> path)                 -- End
    -> (FirstIndex t -> path -> index)  -- Cap
    -> (SecondIndex t -> path -> path)  -- Cons
    -> (index -> a -> f b)
    -> (path -> t b d -> d)
    -> (path -> c -> t a c)
    -> source                           -- End's argument
    -> c -> f d
sdihyloA end cap cons f alg coalg = h . end where
    h p = fmap (alg p) . sbitraverse cap cons f h p . coalg p

sdiisoA
    :: (Applicative f, Diiso s t, BiindexedBitraversable (Dibase s))
    => (source -> path)
    -> (FirstIndex (Dibase s) -> path -> index)
    -> (SecondIndex (Dibase s) -> path -> path)
    -> (index -> a -> f b)
    -> source
    -> s a -> f (t b)
sdiisoA end cap cons f = sdihyloA end cap cons f (const diembed) (const diproject)

sditransA
    :: (Applicative f, Ditrans s t, BiindexedBitraversable (Dibase s))
    => (source -> path)
    -> (FirstIndex (Dibase s) -> path -> index)
    -> (SecondIndex (Dibase s) -> path -> path)
    -> (index -> a -> f b)
    -> (path -> forall x y . Dibase s x y -> Dibase t x y)
    -> source
    -> s a -> f (t b)
sditransA end cap cons f g = sdihyloA end cap cons f (\ p -> diembed . g p) (const diproject)

--
-- Monadic sourced morphisms
--

scataM
    :: (Monad m, IndexedTraversable (Base a), Recursive a)
    => (source -> path)
    -> (Index (Base a) -> path -> path)
    -> (path -> Base a b -> m b)
    -> source
    -> a -> m b
scataM end cons alg = shyloM end cons alg (const projectM)

sanaM
  :: (Monad m, IndexedTraversable (Base b), Corecursive b) =>
     (source -> path)
     -> (Index (Base b) -> path -> path)
     -> (path -> a -> m (Base b a))
     -> source
     -> a
     -> m b
sanaM end cons coalg = shyloM end cons (const embedM) coalg

shyloM
    :: (Monad m, IndexedTraversable f)
    => (source -> path)
    -> (Index f -> path -> path)
    -> (path -> f b -> m b)
    -> (path -> a -> m (f a))
    -> source -> a -> m b
shyloM src cons alg coalg = h . src where h p = alg p <=< straverse cons h p <=< coalg p

-- Trivial, sisoM _ _ _ = isoM
sisoM :: (Monad m, Iso a b, IndexedTraversable (Base a))
    => (source -> path)
    -> (Index (Base a) -> path -> path)
    -> source
    -> a -> m b
sisoM end cons = shyloM end cons (const embedM) (const projectM)

-- NOTE: Inferring the type *will forget* the forall
stransM
    :: (Monad m, Trans s t, IndexedTraversable (Base s))
    => (source -> path)
    -> (Index (Base s) -> path -> path)
    -> (path -> forall a . Base s a -> m (Base t a)) -- Should this forall be before 'path ->'?
    -> source
    -> s -> m t
stransM end cons f = shyloM end cons (\ p -> embedM <=< f p) (const projectM)

--
-- Monadic sourced bimorphisms
--

sbicataM
    :: (Monad m, Birecursive a, BiindexedBitraversable (Bibase a)) 
    => (source -> path)
    -> (FirstIndex (Bibase a) -> path -> path)
    -> (SecondIndex (Bibase a) -> path -> path)
    -> (path -> Bibase a b b -> m b)
    -> source
    -> a -> m b
sbicataM end icons jcons alg = sbihyloM end icons jcons alg (const biprojectM)

sbianaM
    :: (Monad m, Cobirecursive b, BiindexedBitraversable (Bibase b)) 
    => (source -> path)
    -> (FirstIndex (Bibase b) -> path -> path)
    -> (SecondIndex (Bibase b) -> path -> path)
    -> (path -> a -> m (Bibase b a a))
    -> source
    -> a -> m b
sbianaM end icons jcons coalg = sbihyloM end icons jcons (const biembedM) coalg

sbihyloM
    :: (Monad m, BiindexedBitraversable f)
    => (source -> path)                 -- End
    -> (FirstIndex f -> path -> path)   -- Left cons
    -> (SecondIndex f -> path -> path)  -- Right cons
    -> (path -> f b b -> m b)
    -> (path -> a -> m (f a a))
    -> source                           -- End's argument
    -> a -> m b
sbihyloM end icons jcons alg coalg = h . end where
    h p = alg p <=< sbitraverse icons jcons h h p <=< coalg p

sbiisoM
    :: (Monad m, Biiso a b, BiindexedBitraversable (Bibase a))
    => (source -> path)
    -> (FirstIndex (Bibase b) -> path -> path)
    -> (SecondIndex (Bibase b) -> path -> path)
    -> source
    -> a -> m b
sbiisoM end icons jcons = sbihyloM end icons jcons (const biembedM) (const biprojectM)

sbitransM
    :: (Monad m, Bitrans s t, BiindexedBitraversable (Bibase s)) 
    => (source -> path)
    -> (FirstIndex (Bibase s) -> path -> path)
    -> (SecondIndex (Bibase s) -> path -> path)
    -> (path -> forall a b . Bibase s a b -> m (Bibase t a b))
    -> source
    -> s -> m t
sbitransM end icons jcons f = sbihyloM end icons jcons (\ p -> biembedM <=< f p) (const biprojectM)

--
-- Monadic sourced dimorphisms
--

sdicataM
    :: (Monad m, BiindexedBitraversable (Dibase f), Direcursive f)
    => (source -> path)
    -> (FirstIndex (Dibase f) -> path -> index)
    -> (SecondIndex (Dibase f) -> path -> path)
    -> (index -> a -> m b)
    -> (path -> Dibase f b d -> m d)
    -> source
    -> f a -> m d
sdicataM end cap cons f alg  = sdihyloM end cap cons f alg (const diprojectM)

sdianaM
    :: (Monad m, BiindexedBitraversable (Dibase f), Codirecursive f)
    => (source -> path)
    -> (FirstIndex (Dibase f) -> path -> index)
    -> (SecondIndex (Dibase f) -> path -> path)
    -> (index -> a -> m b)
    -> (path -> c -> m (Dibase f a c))
    -> source
    -> c -> m (f b)
sdianaM end cap cons f coalg  = sdihyloM end cap cons f (const diembedM) coalg

sdihyloM
    :: (Monad m, BiindexedBitraversable f)
    => (source -> path)
    -> (FirstIndex f -> path -> index)
    -> (SecondIndex f -> path -> path)
    -> (index -> a -> m b)
    -> (path -> f b d -> m d)
    -> (path -> c -> m (f a c))
    -> source
    -> c -> m d
sdihyloM end cap cons f alg coalg = h . end where
    h p = alg p <=< sbitraverse cap cons f h p <=< coalg p

sdiisoM
    :: (Monad m, Diiso s t, BiindexedBitraversable (Dibase s)) 
    => (source -> path)
    -> (FirstIndex (Dibase s) -> path -> index)
    -> (SecondIndex (Dibase s) -> path -> path)
    -> (index -> a -> m b)
    -> source
    -> s a -> m (t b)
sdiisoM end cap cons f = sdihyloM end cap cons f (const diembedM) (const diprojectM)

sditransM
    :: (Monad m, Ditrans s t, BiindexedBitraversable (Dibase s))
    => (source -> path)
    -> (FirstIndex (Dibase s) -> path -> index)
    -> (SecondIndex (Dibase s) -> path -> path)
    -> (index -> a -> m b)
    -> (path -> forall x y .  Dibase s x y -> m (Dibase t x y))
    -> source
    -> s a -> m (t b)
sditransM end cap cons f g = sdihyloM end cap cons f (\ p -> diembedM <=< g p) (const diprojectM)
