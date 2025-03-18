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

-- A policy maps states to actions
type Policy a = Map (State a) (Action a)

-- A sequence of policies
type PolicySeq a = [Policy a]

type Val = Double

newtype Prob a = Prob {unProb :: [(a, Double)]}
  deriving (Show, Functor)


data State a = State a deriving (Show, Eq, Ord)

data Action a = Action a deriving (Show, Eq, Ord)

runProb :: Eq a => Prob a -> [(a, Double)]
runProb = collectAndSumEqual . unProb

collectAndSumEqual :: Eq a => [(a,Double)] -> [(a, Double)]
collectAndSumEqual aps =
  let support = nub (map fst aps)
  in map (\a -> (a, sum (map snd (filter ((a==).fst) aps))))
         support
         
-- Implementing Monad instance
instance Applicative Prob where
  pure x = Prob [(x, 1)] 
  Prob fs <*> Prob xs = Prob [(f x, p * q) | (f, p) <- fs, (x, q) <- xs]

instance Monad Prob where
  return = pure
  Prob xs >>= f = Prob [(y, p * q) | (x, p) <- xs, (y, q) <- unProb (f x)]

{-
- Reward function

Value is added for transitioning into states which do not have low economic
output and at the same time are not comitted to severe future climate change.
-}
reward :: Int -> State a -> Action a -> State a -> Val
reward _ _ _ next_x = if next_x == DHU || next_x == SHU then 1 else 0


-- OptPolicySeq Function

-- BestExt  
best_ext :: Int -> PolicySeq a -> Policy a
best_ext t ps_tail = Map.fromList $ map bestAction states
  where
    states = [DHU, DHC, DLU, DLC]
    actions = [Start, Delay]

    -- Helper function to determine the best action for a given state
    bestAction state =
      if state `elem` states
        then
          let 
              actionValues = [(action,  (val t (Map.singleton state action : ps_tail) state)) | action <- actions]
              
              best = maximumBy (comparing snd) actionValues
           in (state, fst best)
        else (state, Start) 

-- Define a function for "next" state transition, could be implemented based on your model
next :: Int -> State a -> Action a -> Prob (State a)
next _ _ _ = Prob [(State undefined, 1)]  -- Placeholder, replace with actual transition logic



-- val
val :: Int -> PolicySeq a -> State a -> Val
val _ [] _ = 0
val t (p : ps) x = 
  let y = fromMaybe (Action undefined) (Map.lookup x p)
      mNext = runProb $ next t x y
  in sum [pr * (reward t x y x' + val (t + 1) ps x') | (x', pr) <- mNext]


-- Backwards Induction
bi :: Int -> Int -> PolicySeq a
bi _ 0 = []
bi t n =
  let ps_tail = bi (t + 1) (n - 1)
      p = best_ext t ps_tail
   in p : ps_tail

