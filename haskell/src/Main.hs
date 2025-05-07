module Main where

import Data.Map (Map)
import Data.Map qualified as Map
import SDPCompute
import GHGCase
import Prob(Prob, runProb, Probability)



main :: IO ()
main = putStrLn "Hello, Haskell, unleashed!"

runBest :: Int -> Int -> State -> (Action, Val)
runBest = best ghgcase