-- |
-- Module      : AdvancedStates
-- Description : Climate-Economy SDP Model Implementation
-- Copyright   : (c) Group 12, 2025
-- License     : MIT
--
-- This module implements a Stochastic Dynamic Programming (SDP) model for climate-economy
-- decision making. The model features:
--
-- State Space:
-- * Economic dimension (1..e)
-- * Climate dimension (1..c)
-- * Current action type
--
-- Actions:
-- * MaxEcon  - Prioritize economic growth
-- * MaxClim  - Prioritize climate protection
-- * Passive  - No active intervention
--
-- Transitions:
-- * Normal distribution based
-- * Action-dependent drifts
-- * Bounded state space
--
-- Key Components:
-- * State space generation and management
-- * Action availability rules
-- * Reward function implementation
-- * Transition probability calculations
--
-- The model uses discretized state spaces with configurable granularity (e,c)
-- and implements probabilistic transitions using a normal distribution model
-- with action-dependent drifts.
module AdvancedStates where

import AdvancedProb (cdfNorm)
import Prob (Prob, mkSimpleProb)
import SDPTypes (SDP (SDP), Val)

-- | State space dimensions. These control the granularity of the discretization.
--    - e: Economic dimension size (default: 5)
--    - c: Climate dimension size (default: 5)
e, c :: Int
e = 5 -- economic dimension
c = 5 -- climate dimension

type Time = Int

-- | Actions available to the agent. Each action affects both economic and climate states:
--    - MaxEcon: Prioritize economic growth (positive economic drift, small climate impact)
--    - MaxClim: Prioritize climate protection (negative economic impact, positive climate effect)
--    - Passive: No active intervention (zero drift in both dimensions)
data Action = MaxEcon | MaxClim | Passive
  deriving (Show, Eq, Enum, Ord, Read)

-- | State representation combining:
--    - Current action
--    - Economic state (integer from 1 to e)
--    - Climate state (integer from 1 to c)
--    Plus a None state for error handling.
data State = State Action Int Int | None
  deriving (Show, Eq, Ord, Read)

-- Drifts as a function of action

-- | Compute drift values for economic and climate dimensions based on action.
-- Returns a tuple (economicDrift, climateDrift).
drifts :: Action -> (Val, Val)
drifts action =
  case action of
    MaxEcon -> (1.0, 0.4)
    MaxClim -> (-0.4, 1.0)
    Passive -> (0, 0)

advcase :: SDP State Action
advcase = SDP reward next actions states

getStates :: [State]
getStates = [State x1 x2 x3 | x1 <- [MaxEcon, MaxClim, Passive], x2 <- [1 .. e], x3 <- [1 .. c]]

-- Let x2, x3 roam between 1 and e/c respectively

-- | Generate all possible states for a given time step.
-- Creates the cartesian product of:
-- - Actions [MaxEcon, MaxClim, Passive]
-- - Economic states [1..e]
-- - Climate states [1..c]
states :: Time -> [State]
states _ = getStates

-- | Get all possible actions.
-- Currently returns all actions regardless of state.
getActions :: [Action]
getActions = [MaxEcon, MaxClim, Passive]

-- | Get available actions for a given time and state.
-- Currently returns all actions regardless of state.
actions :: Time -> State -> [Action]
actions _ _ = [MaxEcon, MaxClim, Passive]

-- | Compute reward for transitioning from one state to another.
-- The reward is normalized by e*c and is the product of the economic
-- and climate values in the destination state.
reward :: Time -> State -> Action -> State -> Val
reward _ _ _ (State _ x2' x3')
  | x2' > 0 && x3' > 0 =
      fromIntegral (x2' * x3') / fromIntegral (e * c)
  | otherwise =
      0
reward _ _ _ None = 0

-- | Compute the probability distribution of next states given current state and action.
-- Uses a normal distribution with:
-- - Mean: current value + drift
-- - Standard deviation: sigma (0.5)
-- Probabilities are calculated using the CDF method to ensure proper normalization.
next :: Time -> State -> Action -> Prob State
next _ None _ = error "No next state for None"
next _ (State _ x2 x3) act =
  let sigma = 0.5
      -- \| Compute the probabilities of the next states
      statesWithWeights :: [(State, Double)]
      statesWithWeights =
        let econPairs = zip econEdges pEcon -- [(e, pe)]
            climPairs = zip climEdges pClim -- [(c, pc)]
            -- \| Helper function to clamp values to [1..maxVal]
            clamp :: Int -> Double -> Int
            clamp maxVal x = max 1 (min maxVal (round x))
         in [(State x1' (clamp e e') (clamp c c'), pe * pc) | (e', pe) <- econPairs, (c', pc) <- climPairs]
        where
          x1' = act

          (econDrift, climDrift) = drifts act
          econMu = fromIntegral x2 + econDrift
          climMu = fromIntegral x3 + climDrift

          econEdges, climEdges :: [Double]
          -- \| Include infinities to capture full probability mass
          econEdges = (-1 / 0) : map fromIntegral [1 .. e] ++ [1 / 0]
          climEdges = (-1 / 0) : map fromIntegral [1 .. c] ++ [1 / 0]

          -- \| Calculate probabilities by taking differences of consecutive CDF values
          -- This ensures we capture all probability mass between -∞ and +∞
          pEcon =
            zipWith
              (-)
              (map (\x -> cdfNorm ((x - econMu) / sigma)) (tail econEdges))
              (map (\x -> cdfNorm ((x - econMu) / sigma)) econEdges)

          pClim =
            zipWith
              (-)
              (map (\x -> cdfNorm ((x - climMu) / sigma)) (tail climEdges))
              (map (\x -> cdfNorm ((x - climMu) / sigma)) climEdges)
   in mkSimpleProb statesWithWeights
