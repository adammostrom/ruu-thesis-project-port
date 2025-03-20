{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Theory where

import Data.List (maximumBy, nub)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

-- A policy maps states to actions (simple type, using 'State' and 'Action' as polymorphic)
type Policy a = Map (State a) (Action a)

-- A sequence of policies
type PolicySeq a = [Policy a]

type Val = Double

-- Probability distribution (using a list of values and their probabilities)
newtype Prob a = Prob {unProb :: [(a, Double)]}
  deriving (Show, Functor)

-- State and Action definitions
newtype State a = State a deriving (Show, Eq, Ord)
newtype Action a = Action a deriving (Show, Eq, Ord)

-- Running probability distribution and summing equal values
runProb :: Eq a => Prob a -> [(a, Double)]
runProb = collectAndSumEqual . unProb

collectAndSumEqual :: Eq a => [(a, Double)] -> [(a, Double)]
collectAndSumEqual aps =
  let support = nub (map fst aps)
  in map (\a -> (a, sum (map snd (filter ((a ==) . fst) aps))))
         support

-- Reward function
-- Refactor 20 March: Made it undefined as it is not defined in the Idris code, and Theory should actually serve more as an interface.
reward :: Int -> State a -> Action a -> State a -> Val
reward _ _ _ next_x = undefined

-- Define a function for state transitions (for now, a placeholder for actual logic)
next :: Int -> State a -> Action a -> Prob (State a)
next _ _ _ = Prob [(State undefined, 1)] 

-- Value function for a policy sequence
val :: Eq a => Ord a => Int -> PolicySeq a -> State a -> Val
val _ [] _ = 0
val t (p : ps) x = 
  let y = fromMaybe (Action undefined) (Map.lookup x p)
      mNext = runProb $ next t x y
  in sum [pr * (reward t x y x' + val (t + 1) ps x') | (x', pr) <- mNext]

mkSimpleProb :: [(State a, Double)] -> Prob (State a)
mkSimpleProb = undefined

best :: Int -> Int -> State a -> (Action a, Val)
best = undefined

mMeas :: Int -> Int -> State a -> Double
mMeas = undefined

{- bestExt :: Int -> PolicySeq (State a) -> Policy (State a)
bestExt = undefined -}

class BestExt a where
  bestExt :: Int -> PolicySeq (State a) -> Policy (State a)

-- Backwards induction - computes the optimal policy sequence
bi :: BestExt a => Int -> Int -> PolicySeq (State a)
bi _ 0 = []
bi t n =
  let ps_tail = bi t (n - 1)
      p = bestExt t ps_tail
  in p : ps_tail

