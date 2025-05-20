module Interface where

import Data.Time.Clock
import SDPCompute
import GHGCase

-- Load SDP instance
sdpInstance :: SDP State Action
sdpInstance = ghgcase

-- Entry point
main :: IO ()
main = do
  putStrLn "Greenhouse Gas SDP Interface Loaded."
  putStrLn "Type `help` for usage information."

-- HELP DIALOG
help :: IO ()
help = do
  putStrLn "\n Greenhouse Gas SDP - Available Commands:\n"
  putStrLn "  runBest :: Int -> Int -> State -> (Action, Val)"
  putStrLn "    - Computes the optimal action and value at a given timestep and horizon."
  putStrLn "    - Usage: runBest <timeStep> <horizon> <state>"

  putStrLn "\n  runBestMemo :: Int -> Int -> State -> (Action, Val)"
  putStrLn "    - Same as runBest, but uses memoization for performance."

  putStrLn "\n  runmMeas :: Int -> Int -> State -> Val"
  putStrLn "    - Returns the measurement value (Val) at a given timestep, horizon, and state."

  putStrLn "\n  timeRunBest :: Int -> Int -> State -> IO ()"
  putStrLn "    - Benchmarks runtime of `runBest` and prints execution time."

  putStrLn "\n  timeRunBestMemo :: Int -> Int -> State -> IO ()"
  putStrLn "    - Benchmarks runtime of `runBestMemo` and prints execution time."

  putStrLn "\n Notes:"
  putStrLn "  - `State` and `Action` are domain-specific Enums imported from the GHGCase module."
  putStrLn "  - Type `:t <function>` in GHCi to inspect types."

-- Core functions
runBest :: Int -> Int -> State -> (Action, Val)
runBest = best sdpInstance

runBestMemo :: Int -> Int -> State -> (Action, Val)
runBestMemo = best' sdpInstance

runmMeas :: Int -> Int -> State -> Val
runmMeas = mMeas sdpInstance


runmMeasMemo :: Int -> Int -> State -> Val
runmMeasMemo = mMeas' sdpInstance

runBi :: Int -> Int -> PolicySeq State Action
runBi = bi sdpInstance

runVal :: Int -> PolicySeq State Action -> State -> Val
runVal = val sdpInstance

runBestExt :: Int -> PolicySeq State Action -> Policy State Action
runBestExt = bestExt sdpInstance

runWorstExt :: Int -> PolicySeq State Action -> Policy State Action
runWorstExt = bestExt sdpInstance

-- Benchmarking utilities
timeRun :: (Int -> Int -> State -> (Action, Val)) -> Int -> Int -> State -> IO ()
timeRun runFunction timeStep horizon state = do
  startTime <- getCurrentTime
  let (a, v) = runFunction timeStep horizon state
  endTime <- getCurrentTime
  let diff = diffUTCTime endTime startTime
  putStrLn $ "\n(a, v): " ++ show (a, v)
  putStrLn $ "Execution Time: " ++ show diff ++ "\n"

timeRunBest :: Int -> Int -> State -> IO ()
timeRunBest = timeRun runBest

timeRunBestMemo :: Int -> Int -> State -> IO ()
timeRunBestMemo = timeRun runBestMemo