{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies, KindSignatures #-}

module Theory where

import Data.List (maximumBy, minimumBy, nub)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
{-|
Module      : Theory
Description : A module defining a theoretical framework for decision-making processes, including policies, states, actions, and probability distributions.
Stability   : experimental
Portability : POSIX

This module provides a typeclass `Theory` for defining decision-making models, along with utility functions for working with policies, states, actions, and probability distributions. It also includes an implementation of backwards induction for solving decision problems.

Types:
  - `Val`: Represents a numerical value, typically used for rewards or evaluations.
  - `Policy x y`: A mapping from states (`x`) to actions (`y`).
  - `PolicySeq x y`: A sequence of policies over time.
  - `Prob a`: A probability distribution monad for representing probabilistic outcomes.

Typeclass:
  - `Theory x y`: A typeclass defining the core components of a decision-making model:
      - `reward`: Computes the reward for transitioning between states given an action.
      - `next`: Defines the probability distribution over next states given a current state and action.
      - `actions`: Lists all possible actions for a given state and time step.
      - `states`: Lists all possible states in the model.

Functions:
  - `runProb`: Normalizes and collects probabilities for a given `Prob` distribution.
  - `mkSimpleProb`: Constructs a `Prob` distribution, ensuring non-negative probabilities.
  - `bi`: Implements backwards induction to compute a sequence of optimal policies.
  - `val`: Computes the value of a state under a given policy sequence.
  - `best`: Determines the best action and its value for a given state and horizon.
  - `mMeas`: Computes a measure of the difference between the best and worst policies for a state.
  - `bestExt`: Constructs the best policy for a given time step and policy sequence.
  - `worstExt`: Constructs the worst policy for a given time step and policy sequence.
  - `headPolicy`: Retrieves the first policy from a policy sequence, if available.
  - `tailPolicy`: Retrieves the tail of a policy sequence, if available.

Utilities:
  - `collectAndSumEqual`: Helper function to combine probabilities for identical outcomes.
  - `getBestAction`: Helper function to determine the best action for a state.
  - `getWorstAction`: Helper function to determine the worst action for a state.

This module is designed for use in decision-making and reinforcement learning scenarios, where policies and value functions are computed iteratively over a finite horizon.
-}

type Val = Double

type Probability = Double

type Policy x y = Map x y

type PolicySeq x y = [Policy x y]


-- Theory Typeclass
class Theory x y where

  reward :: Int -> x -> y -> x -> Val

  next :: Int -> x -> y -> Prob x

  actions :: Int -> x -> [y]
  
  -- "Bogus" y in order to have correct type parameters
  states :: y ->  [x]


-- Probability distribution monad, (State, Probability)
newtype Prob a = Prob {unProb :: [(a, Probability)]}
  deriving (Show, Functor)

-- helper function to extract all the probabilities from the uncertainty monad
weights :: Prob a -> [Probability]
weights (Prob xs) = map snd xs

-- No negative probabilities.
validProb :: Prob a -> Bool
validProb (Prob xs) =
  let ws = weights (Prob xs) in all (>0) ws && sum ws > 0 

normalize :: Prob a -> Prob a
normalize (Prob xs) = 
  let total = sum (map snd xs )
  in if total == 0 then error "Total probability equals to zero, cannot normalize"
     else Prob [(x, p / total) | (x,p) <- xs]

prob :: (Eq a) => Prob a -> a -> Probability
prob (Prob xs) x = 
  let total = sum (map snd xs)
      matching = sum [p | (x', p) <- xs, x == x']
  in if total == 0 then 0 else matching / total

-- Implementing Monad instance
instance Applicative Prob where
  pure x = Prob [(x, 1)]
  Prob fs <*> Prob xs = Prob [(f x, p * q) | (f, p) <- fs, (x, q) <- xs]

instance Monad Prob where
  return = pure
  Prob xs >>= f = normalize . Prob $ [(y, p * q) | (x, p) <- xs, let Prob ys = normalize (f x), (y, q) <- ys]

runProb :: (Eq a) => Prob a -> [(a, Probability)]
runProb = collectAndSumEqual . unProb

collectAndSumEqual :: (Eq a) => [(a, Probability)] -> [(a, Probability)]
collectAndSumEqual aps =
  let support = nub (map fst aps)
   in map
        (\a -> (a, sum (map snd (filter ((a ==) . fst) aps))))
        support

-- Helper: Ensure probabilities are non-negative
mkSimpleProb :: [(x, Probability)] -> Prob x
mkSimpleProb = Prob . filter (\(_, p) -> p >= 0)

-- Backwards induction
bi ::(Theory x y, Show x, Eq x, Ord x) =>  Int -> Int -> PolicySeq x y
bi _ 0 = []
bi t n =
  let ps_tail = bi t (n - 1)
      p = bestExt t ps_tail
   in p : ps_tail


   -- skapa lista av värden -> ta measure på result (reward ...)
val :: (Theory x y, Ord x, Show x) => Int -> PolicySeq x y -> x -> Val
val _ [] _ = 0
val t (p : ps) x =
  case Map.lookup x p of
    Nothing -> error $ "No action found in policy for state: " ++ show x
    Just y ->
      let mNext = runProb $ next t x y
      in sum [pr * (reward t x y x' + val (t + 1) ps x') | (x', pr) <- mNext]


best :: (Theory x y, Show x, Eq x, Ord x) => Int -> Int -> x -> (y, Val)
best t n x
    | n == 0 = error "Horizon must be greater than zero!"
    | otherwise =
        let ps = bi (t + 1) (n - 1)
            p = bestExt t ps
            b = fromMaybe (error "No action found!") (Map.lookup x p)
            vb = val t (p : ps) x
        in (b, vb)


headPolicy :: PolicySeq x y-> Maybe (Policy x y)
headPolicy [] = Nothing
headPolicy (p:ps) = Just p

tailPolicy :: PolicySeq x y-> Maybe (PolicySeq x y)
tailPolicy [] = Nothing
tailPolicy (p:ps) = Just ps


-- PJ : ( asTypeOf states $ head ps_tail)
bestExt :: (Theory x y, Show x, Eq x, Ord x) =>  Int -> PolicySeq x y -> Policy x y
bestExt  t ps_tail = tSt `asTypeOf`head ps_tail
  where
    y = snd $ head (Map.toList $ head ps_tail)
    tSt = Map.fromList $ map bestAction (states y)
    bestAction state = (state, getBestAction t state ps_tail)
    getBestAction t state ps_tail =
      let actionsForState = actions t state
          actionValues = [(action, val t (Map.singleton state action : ps_tail) state) | action <- actionsForState]
          best = maximumBy (comparing snd) actionValues
        in fst best


worstExt :: (Theory x y, Show x, Eq x, Ord x) =>  Int -> PolicySeq x y -> Policy x y
worstExt  t ps_tail =  tSt `asTypeOf` head ps_tail
  where
    y = snd $ head (Map.toList $ head ps_tail)
    tSt = Map.fromList $ map worstAction (states y)
    worstAction state = (state, getWorstAction t state ps_tail)
    getWorstAction t state ps_tail =
      let actionsForState = actions t state
          actionValues = [(action, val t (Map.singleton state action : ps_tail) state) | action <- actionsForState]
          worst = minimumBy (comparing snd) actionValues
      in fst worst 