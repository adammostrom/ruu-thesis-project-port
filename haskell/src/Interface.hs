module Interface where

import Data.Time.Clock


import SDPCompute
import GHGCase
--import AdvancedStates

-- Main.hs

sdpInstance :: SDP State Action
sdpInstance   = ghgcase

main :: IO ()
main = do
  putStrLn "Loaded module MatterMost.\n Exposed functions: \n * runBest :: Int -> Int -> State -> (Action, Val)"







runBest :: Int -> Int -> State -> (Action, Val)
runBest = best sdpInstance

runmMeas :: Int -> Int -> State -> Val
runmMeas = mMeas sdpInstance

runBestMemo :: Int -> Int -> State -> (Action, Val)
runBestMemo = best' sdpInstance


timeRunBest :: Int -> Int -> State -> IO ()
timeRunBest decision_step horizon x = do
    startTime <- getCurrentTime
    (a, v) <- return (runBest decision_step horizon x)
    putStrLn $ "(a, v): " ++ (show (a, v))
    endTime <- getCurrentTime
    let diff = diffUTCTime endTime startTime
    putStrLn $ "Execution Time: " ++ (show diff)