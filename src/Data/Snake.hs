module Data.Snake where

import Prelude

import Data.Bifunctor
import Data.Functor
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Control.Applicative
import Data.Function
import Data.Tuple

import Data.Indexed
import Data.Pathed
import Data.Biindexed
import Data.Dipathed

import Data.Sourced

-- data NonEmpty a = NonEmpty a [a] -- NonEmpty a ~ (a,[a])

data Cap head body = Cap head [body]

infixr 5 :<
pattern (:<) :: head -> [body] -> Cap head body
pattern head :< body = Cap head body

exampleCap :: Cap String Int
exampleCap = "head" :< 1 : 2 : 3 : []

data Way body tail = Way body (Way body tail) | End tail

infixr 5 :-
pattern (:-) :: body -> Way body tail -> Way body tail
pattern body :- way = Way body way

infixr 5 :>
pattern (:>) :: body -> tail -> Way body tail
pattern body :> tail = Way body (End tail)

exampleWay :: Way Int String
exampleWay = 1 :- 2 :- 3 :> "tail"

data Snake head body tail = Snake head (Way body tail)

infixr 5 :|
pattern (:|) :: head -> Way body tail -> Snake head body tail
pattern head :| body = Snake head body

exampleSnake :: Snake String Int ()
exampleSnake = "head" :| 1 :- 2 :- 3 :> ()

tuplehylo
    :: (IndexedFunctor f)
    => (([Index f], source) -> f b -> b)
    -> (([Index f], source) -> a -> f a)
    -> source
    -> a
    -> b
tuplehylo = shylo ([],) (first . (:))

tripledihylo
    :: (BiindexedBifunctor f)
    => ((FirstIndex f, [SecondIndex f], source) -> a -> b)
    -> (([SecondIndex f], source) -> f b d -> d)
    -> (([SecondIndex f], source) -> c -> f a c)
    -> source
    -> c
    -> d
tripledihylo = sdihylo ([],) (\ i (p,s) -> (i,p,s)) (first . (:))

capdihylo
    :: BiindexedBifunctor f
    => (Cap (FirstIndex f) (SecondIndex f) -> a -> b)
    -> ([SecondIndex f] -> f b d -> d)
    -> ([SecondIndex f] -> c -> f a c)
    -> [SecondIndex f] -> c -> d
capdihylo = pdihylo Cap (:)

wayhylo
    :: (IndexedFunctor f)
    => (Way (Index f) source -> f b -> b)
    -> (Way (Index f) source -> a -> f a)
    -> source
    -> a
    -> b
wayhylo = shylo End Way

snakedihylo
    :: BiindexedBifunctor f
    => (Snake (FirstIndex f) (SecondIndex f) source -> a -> b)
    -> (Way (SecondIndex f) source -> f b d -> d)
    -> (Way (SecondIndex f) source -> c -> f a c)
    -> source -> c -> d
snakedihylo = sdihylo End Snake Way

doublesnakedihylo
    :: (BiindexedBifunctor f, BiindexedBifunctor g)
    => (Snake (FirstIndex g) (SecondIndex g) (Snake (FirstIndex f) (SecondIndex f) source) -> x -> y)
    -> (Way (SecondIndex g) (Snake (FirstIndex f) (SecondIndex f) source) -> g y b -> b)
    -> (Way (SecondIndex g) (Snake (FirstIndex f) (SecondIndex f) source) -> a -> g x a)
    -> (Way (SecondIndex f) source -> f b d -> d)
    -> (Way (SecondIndex f) source -> c -> f a c)
    -> source
    -> c
    -> d
doublesnakedihylo g galg gcoalg falg fcoalg = snakedihylo (snakedihylo g galg gcoalg) falg fcoalg
