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
data State a = State a deriving (Show, Eq, Ord)
data Action a = Action a deriving (Show, Eq, Ord)

-- Running probability distribution and summing equal values
runProb :: Eq a => Prob a -> [(a, Double)]
runProb = collectAndSumEqual . unProb

collectAndSumEqual :: Eq a => [(a, Double)] -> [(a, Double)]
collectAndSumEqual aps =
  let support = nub (map fst aps)
  in map (\a -> (a, sum (map snd (filter ((a ==) . fst) aps))))
         support

-- Reward function - add value for certain transitions (you can adapt the logic as needed)
reward :: Int -> State a -> Action a -> State a -> Val
reward _ _ _ next_x = if next_x == DHU || next_x == SHU then 1 else 0

-- Define a function for state transitions (for now, a placeholder for actual logic)
next :: Int -> State a -> Action a -> Prob (State a)
next _ _ _ = Prob [(State undefined, 1)]  -- Placeholder, replace with actual transition logic

-- Value function for a policy sequence
val :: Int -> PolicySeq a -> State a -> Val
val _ [] _ = 0
val t (p : ps) x = 
  let y = fromMaybe (Action undefined) (Map.lookup x p)
      mNext = runProb $ next t x y
  in sum [pr * (reward t x y x' + val (t + 1) ps x') | (x', pr) <- mNext]

-- Backwards induction - computes the optimal policy sequence
bi :: Int -> Int -> PolicySeq a
bi _ 0 = []
bi t n =
  let ps_tail = bi (t + 1) (n - 1)
      p = best_ext t ps_tail  -- Assuming 'best_ext' is defined elsewhere
  in p : ps_tail
