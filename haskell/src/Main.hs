module Main where

import Data.Time.Clock


import SDPCompute
import GHGCase
--import AdvancedStates


runBest :: Int -> Int -> State -> (Action, Val)
runBest = best ghgcase

runmMeas :: Int -> Int -> State -> Val
runmMeas = mMeas ghgcase

runBestMemo :: Int -> Int -> State -> (Action, Val)
runBestMemo = best' ghgcase


timeRunBest :: Int -> Int -> State -> IO ()
timeRunBest decision_step horizon x = do
   startTime <- getCurrentTime
   (a, v) <- return (runBest decision_step horizon x)
   putStrLn $ "(a, v): " ++ show (a, v)
   endTime <- getCurrentTime
   let diff = diffUTCTime endTime startTime
   putStrLn $ "Execution Time: " ++ show diff


main :: IO ()
main = do
    putStrLn "Haskell Unleashed!"
    putStrLn "Available implementations to test: \n 0. GHGCase \n 1. AdvancedStates "
    input <- getLine

    case input of
      "0" -> do ghgcase
      "1" -> do advcase
      _ -> putStrLn "No module for that number"

ghgcase :: IO()
ghgcase = do
    putStrLn "Available functions: * runBest \n * runmMeas \n * runBestMemo \n * timeRunBest \n "


