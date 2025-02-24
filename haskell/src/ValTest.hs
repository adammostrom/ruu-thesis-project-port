module ValTest where

{-# LANGUAGE RankNTypes #-}

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

data Action = Start | Delay | Unit
  deriving (Show, Eq, Enum, Ord)

data State = DHU | DHC | DLU | DLC | SHU | SHC | SLU | SLC
  deriving (Show, Eq, Enum, Ord)

type Val = Double 

-- A policy maps states to actions
type Policy = Map State Action

-- A sequence of policies
type PolicySeq = [Policy]

-- Helper: Ensure probabilities are non-negative
mkSimpleProb :: [(State, Double)] -> Map State Double
mkSimpleProb = Map.fromList . filter (\(_, p) -> p >= 0)

-- Reward function (now includes `next_x` like Python)
reward :: Int -> State -> Action -> State -> Val
reward _ _ _ next_x = if next_x == DHU || next_x == SHU then 1 else 0

-- State transition function: returns a probability distribution over next states
next :: Int -> State -> Action -> Map State Double
next _ _ _ = mkSimpleProb [(DHU, 0.5), (DHC, 0.5)]  -- Placeholder

-- Measurement function: computes expectation
meas :: (State -> Val) -> Map State Double -> Val
meas f dist = sum [p * f x | (x, p) <- Map.toList dist]

-- Value function (recursive computation)
val :: Int -> PolicySeq -> State -> Val
val _ [] _ = 0
val t (p:ps) x =
    let y    = fromMaybe Unit (Map.lookup x p)  -- Default action if missing
        mx'  = next t x y
    in meas (\x' -> reward t x y x' + val (t + 1) ps x') mx'

-- Compute the best extension of a policy sequence
best_ext :: Int -> PolicySeq -> Policy
best_ext t ps_tail = Map.fromList $ map bestAction states
  where
    states = [DHU, DHC, DLU, DLC]  -- Only these states have actions
    actions = [Start, Delay]
    
    bestAction state =
        let best = maximum [(val t (Map.singleton state action : ps_tail) state, action) | action <- actions]
        in (state, snd best)

-- Recursively build an optimal policy sequence
bi :: Int -> Int -> PolicySeq
bi _ 0 = []
bi t n = let ps_tail = bi (t + 1) (n - 1)
             p = best_ext t ps_tail
         in p : ps_tail

-- Compute the best action and expected value for a given time, state, and horizon
best :: Int -> Int -> State -> (Action, Val)
best t n x
    | n == 0 = error "Horizon must be greater than zero!"
    | otherwise =
        let ps = bi (t + 1) (n - 1)
            p = best_ext t ps
            b = fromMaybe Unit (Map.lookup x p)
            vb = val t (p : ps) x
        in (b, vb)

-- Compute how much a state matters for optimal decision-making
mMeas :: Int -> Int -> State -> Double
mMeas t n x
    | x `elem` [SHU, SHC, SLU, SLC] = 0
    | otherwise =
        let ps = bi t n
            ps' = if Map.lookup x (head ps) == Just Start
                  then (Map.insert x Delay (head ps)) : tail ps
                  else (Map.insert x Start (head ps)) : tail ps
            bestVal = val t ps x
            worstVal = val t ps' x
        in (bestVal - worstVal) / bestVal
