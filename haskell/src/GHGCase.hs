{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : GHGCase
Description : Defines the GHGCase module for modeling state-action transitions in a stochastic dynamic programming framework.
License     : MIT
Stability   : experimental
Portability : POSIX

This module provides the implementation of the GHGCase problem, including
state definitions, actions, transition probabilities, and reward functions.
It is designed for use in stochastic dynamic programming applications.
-}

module GHGCase where

import GHGCaseParam
import Prob (Prob, mkSimpleProb)
import SDPTypes (SDP (SDP), Val)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

-- | Actions available for GHG case.
data Action = Start | Delay | Unit
  deriving (Show, Eq, Enum, Ord, Read, NFData, Generic)

-- | Represent micro-states for a green transition, corresponding to the states as defined in the original paper.
data State = DHU | DHC | DLU | DLC | SHU | SHC | SLU | SLC
  deriving (Show, Eq, Enum, Ord, Read, NFData, Generic)

-- | An SDP instance modeling the GHG emission scenario
ghgcase :: SDP State Action
ghgcase = SDP reward next actions states

-- | Return all States.
getStates :: [State]
getStates = [DHU, DHC, DLU, DLC, SHU, SHC, SLU, SLC]

-- | Return all Actions.
getActions :: [Action]
getActions = [Start, Delay, Unit]

-- | Concrete implementation of SDP type function. Case dependent. Return available states at a given timestep.
states :: Int -> [State]
states _ = getStates

-- | Concrete implementation of SDP type function. Case dependent. Return available actions at a certain time step and a specific state.
actions :: Int -> State -> [Action]
actions _ x
  | x `elem` [DHU, DHC, DLU, DLC] = [Start, Delay]
  | x `elem` [SHU, SHC, SLU, SLC] = [Unit]
  | otherwise = error "no legitimate states"

-- | Concrete implementation of SDP type function. Case dependent.
reward :: Int -> State -> Action -> State -> Val
reward _ _ _ next_x = if next_x == DHU || next_x == SHU then 1 else 0



-- | Concrete implemenation of SDP type function. Case dependent. Returns the probability distribution of the next state, given a state, a timestep and an action to take.
next :: Int -> State -> Action -> Prob State
next t x y = case x of
  DHU -> nextDU t y pH_D_DH pL_D_DH pH_S_DH pL_S_DH
  DLU -> nextDU t y pH_D_DL pL_D_DL pH_S_DL pL_S_DL
  DHC -> nextDC y pH_D_DH pL_D_DH pH_S_DH pL_S_DH
  DLC -> nextDC y pH_D_DH pL_D_DH pH_S_DH pL_S_DH
  SHU -> nextSU t pH_S_SH pL_S_SH
  SLU -> nextSU t pH_S_SL pL_S_SL
  SHC -> nextSC 
  SLC -> nextSC 

-- | Helper function for next, returns probability distribution if current state is DHU or DLU.
nextDU :: Int -> Action -> Double -> Double -> Double -> Double -> Prob State
nextDU t y phd pld phs pls =
  mkSimpleProb
    [ (DHU, fst action * phd * nextCheckTime t DHU),
      (DHC, fst action * phd * nextCheckTime t DHC),
      (DLU, fst action * pld * nextCheckTime t DLU),
      (DLC, fst action * pld * nextCheckTime t DLC),
      (SHU, snd action * phs * nextCheckTime t SHU),
      (SHC, snd action * phs * nextCheckTime t SHC),
      (SLU, snd action * pls * nextCheckTime t SLU),
      (SLC, snd action * pls * nextCheckTime t SLC)
    ]
  where
    action = checkAction y

-- | Helper function for next, returns probability distribution if current state is DHC or DLC
nextDC :: Action -> Double -> Double -> Double -> Double -> Prob State
nextDC y phd pld phs pls =
  mkSimpleProb
    [ (DHC, fst action * phd),
      (DLC, fst action * pld),
      (SHC, snd action * phs),
      (SLC, snd action * pls)
    ]
  where
    action = checkAction y

-- | Helper function for next, returns probability distribution if current state is SHU or SLU
nextSU :: Int -> Double -> Double -> Prob State
nextSU t phs pls =
  mkSimpleProb
    [ (SHU, phs * nextCheckTime t SHU),
      (SHC, phs * nextCheckTime t SHC),
      (SLU, pls * nextCheckTime t SLU),
      (SLC, pls * nextCheckTime t SLC)
    ]
-- | Helper function for next, returns probability distribution if current state is SHC or SLC
nextSC ::  Prob State
nextSC  =
  mkSimpleProb
    [ (SHC, pH_S_SL),
      (SLC, pL_S_SL)
    ]

-- | Helper function to return the value given a state and a time > 0
nextTime :: State -> Double
nextTime state = checkState state pU_D pC_D pU_S pC_S

-- | Helper function to return the value given a state and when timestep is 0
nextTimeZero :: State -> Double
nextTimeZero state = checkState state pU_D_0 pC_D_0 pU_S_0 pC_S_0

-- | Utility function by the next helper functions to take the parameter t and delegate to the correct time function.
nextCheckTime :: Int -> State -> Double
nextCheckTime t
  | t < 0 = error "Time cannot be negative"
  | t == 0 = nextTimeZero
  | otherwise = nextTime

-- | Helper function to return the correct value corresponding to the action given.
checkAction :: Action -> (Double, Double)
checkAction y
  | y == Start = (pD_Start, pS_Start)
  | y == Delay = (pD_Delay, pS_Delay)
  | otherwise = error "Not a valid action"

-- | Helper function returning the value given a State.
checkState :: State -> Double -> Double -> Double -> Double -> Double
checkState state pud pcd pus pcs = case state of
  DHU -> pud
  DHC -> pcd
  DLU -> pud
  DLC -> pcd
  SHU -> pus
  SHC -> pcs
  SLU -> pus
  SLC -> pcs