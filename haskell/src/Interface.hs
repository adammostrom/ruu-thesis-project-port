module Interface where

import Data.Map (Map)
import Data.Map qualified as Map
import Prob(Prob, runProb, Probability)

-- Import SDP functions
import SDPCompute

-- Import the SDP module
import GHGCase


runBest :: Int -> Int -> State -> (Action, Val)
runBest = best ghgcase

runmMeas :: Int -> Int -> State -> Val
runmMeas = mMeas ghgcase