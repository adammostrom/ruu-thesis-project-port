module Interface where

-- Import SDP functions
import SDPCompute

-- Import the SDP module
import GHGCase


runBest :: Int -> Int -> State -> (Action, Val)
runBest = best ghgcase

runmMeas :: Int -> Int -> State -> Val
runmMeas = mMeas ghgcase