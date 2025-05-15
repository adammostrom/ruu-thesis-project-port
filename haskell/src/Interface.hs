module Interface where

import Data.Time.Clock


import SDPCompute
import GHGCase


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
