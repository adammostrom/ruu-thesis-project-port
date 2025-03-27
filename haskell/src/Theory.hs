{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Theory where

import Data.List (nub)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

-- Core Types
type Val = Double

-- Define abstract types for State and Action
type Policy s a = Map (State s) (Action a)

type PolicySeq s a = [Policy s a]

newtype State s = State s deriving (Show, Eq, Ord)

newtype Action a = Action a deriving (Show, Eq, Ord)

newtype Prob a = Prob {unProb :: [(a, Double)]} deriving (Show, Functor)

instance Applicative Prob where
    pure x = Prob [(x, 1)]
    Prob fs <*> Prob xs = Prob [(f x, p * q) | (f, p) <- fs, (x, q) <- xs]

instance Monad Prob where
    return = pure
    Prob xs >>= f = Prob [(y, p * q) | (x, p) <- xs, (y, q) <- unProb (f x)]

runProb :: Eq a => Prob a -> [(a, Double)]
runProb = collectAndSumEqual . unProb

collectAndSumEqual :: Eq a => [(a, Double)] -> [(a, Double)]
collectAndSumEqual aps =
    let support = nub (map fst aps)
    in map (\a -> (a, sum (map snd (filter ((a ==) . fst) aps)))) support

-- Theory class now abstracts over State and Action types
class (Ord (State s), Eq (State s), Ord (Action a), Eq (Action a)) => Theory s a where
    reward :: Int -> State s -> Action a -> State s -> Val
    next   :: Int -> State s -> Action a -> Prob (State s)
    bestExt :: Int -> PolicySeq s a -> Policy s a

    reward _ _ _ _ = undefined
    next _ _ _ = Prob [(State undefined, 1)]




-- Helper: Ensure probabilities are non-negative
mkSimpleProb :: [(State s, Double)] -> Prob (State s)
mkSimpleProb = Prob . filter (\(_, p) -> p >= 0)


val ::(Eq s) => (Ord a) => (Ord s) => (Theory s a) => Int -> PolicySeq s a -> State s -> Val
val _ [] _ = 0
val t (p : ps) x =
    let y = fromMaybe (Action undefined) (Map.lookup x p)
        mNext = runProb $ next t x y
    in sum [pr * (reward t x y x' + val (t + 1) ps x') | (x', pr) <- mNext]

-- Backwards induction
bi :: Int -> Int -> PolicySeq s a
bi _ 0 = []
bi t n =
    let ps_tail = bi t (n - 1)
        p = bestExt t ps_tail
    in p : ps_tail


