{-|
Module      : Prob
Description : Probability Monad Implementation
Copyright   : (c) Group 12, 2025
License     : MIT

This module implements a probability monad for handling discrete probability distributions.
Key features:

Core Functionality:
* Probability Distribution Type
  - Represents discrete probability distributions
  - Supports arbitrary value types
  - Implements Functor, Applicative, and Monad

Operations:
* Distribution Manipulation
  - Normalization
  - Probability extraction
  - Distribution combination
  - Expectation calculation

Utility Functions:
* weights     - Extract probability weights
* validProb   - Check distribution validity
* normalize   - Normalize probabilities to sum to 1
* mkSimpleProb - Create valid probability distributions

Implementation Details:
- Uses list-based representation [(value, probability)]
- Ensures non-negative probabilities
- Maintains probability sum invariants
- Thread-safe pure functional implementation
-}
{-# LANGUAGE DeriveFunctor #-}
module Prob where
import Data.List (maximumBy, minimumBy, nub)

{-| Type alias for probability values in [0,1] -}
type Probability = Double

{-| Probability distribution monad.
    Represents a discrete probability distribution over values of type 'a'.
    
    Internal representation: [(value, probability)]
    Invariants:
    * All probabilities are non-negative
    * Sum of probabilities is positive
-}
newtype Prob a = Prob {unProb :: [(a, Probability)]}
  deriving (Show, Functor)

{-| Extract probability weights from a distribution.
    
    >>> weights (Prob [(True, 0.3), (False, 0.7)])
    [0.3, 0.7]
-}
weights :: Prob a -> [Probability]
weights (Prob xs) = map snd xs

{-| Check if a probability distribution is valid.
    
    Properties:
    * All probabilities are positive
    * Sum of probabilities is positive
-}
validProb :: Prob a -> Bool
validProb (Prob xs) =
  let ws = weights (Prob xs) in all (>0) ws && sum ws > 0

{-| Normalize a probability distribution to sum to 1.
    
    >>> normalize (Prob [(True, 2), (False, 3)])
    Prob [(True, 0.4), (False, 0.6)]
    
    Throws an error if total probability is zero.
-}
normalize :: Prob a -> Prob a
normalize (Prob xs) =
  let total = sum (map snd xs)
  in if total == 0 then error "Total probability equals to zero, cannot normalize"
     else Prob [(x, p / total) | (x,p) <- xs]

{-| Get the probability of a specific outcome.
    
    Parameters:
    * dist - The probability distribution
    * x    - The outcome to query
    
    Returns normalized probability of x in dist.
-}
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

{-| Run a probability computation, collecting and normalizing results.
    Combines equal outcomes by summing their probabilities.
    
    >>> runProb (Prob [(True, 0.3), (True, 0.2), (False, 0.5)])
    [(True, 0.5), (False, 0.5)]
-}
runProb :: (Eq a) => Prob a -> [(a, Probability)]
runProb = collectAndSumEqual . unProb

collectAndSumEqual :: (Eq a) => [(a, Probability)] -> [(a, Probability)]
collectAndSumEqual aps =
  let support = nub (map fst aps)
   in map
        (\a -> (a, sum (map snd (filter ((a ==) . fst) aps))))
        support

-- Helper: Ensure probabilities are non-negative
{-| Create a valid probability distribution from a list of outcomes and probabilities.
    Filters out negative probabilities.
    
    >>> mkSimpleProb [(True, 0.3), (False, -0.1), (True, 0.7)]
    Prob [(True, 0.3), (True, 0.7)]
-}
mkSimpleProb :: [(x, Probability)] -> Prob x
mkSimpleProb = Prob . filter (\(_, p) -> p >= 0)

{-| Calculate the expected value of a function over a probability distribution.
    
    Parameters:
    * f    - Function to apply to outcomes
    * dist - Probability distribution
    
    Returns E[f(X)] where X follows the given distribution
-}
expectation :: Eq a => (a -> Double) -> Prob a -> Double
expectation f aps = sum (map (\(a, pr) -> pr * f a) (runProb aps))
