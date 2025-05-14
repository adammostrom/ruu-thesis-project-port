{-|
Module      : SDPTypes
Description : Core types for Stochastic Dynamic Programming
Copyright   : (c) Group 12, 2025
License     : MIT

This module defines the fundamental types and data structures for 
Stochastic Dynamic Programming (SDP). It provides:

Type Definitions:
* Val       - Numeric type for values and rewards
* Policy    - Maps states to actions
* PolicySeq - Sequences of policies over time

Core Data Structure:
* SDP - Record type containing:
  - reward   : Reward function for transitions
  - next     : State transition probability function
  - actions  : Available actions for each state
  - states   : State space at each time step

Type Parameters:
* x - State type
* y - Action type

All functions support arbitrary state and action types,
allowing for flexible model implementations.
-}
module SDPTypes where
import Data.Map (Map)
import Prob(Prob)
type Val = Double
type Policy x y = Map x y -- Möjligen sätta (Map x y, Map x Val)
type PolicySeq x y = [Policy x y]
-- SDP record
data SDP x y = SDP {
     reward   :: Int -> x -> y -> x -> Val
  ,  next     :: Int -> x -> y -> Prob x
  ,  actions  :: Int -> x -> [y]
  ,  states   :: Int -> [x]
  }
