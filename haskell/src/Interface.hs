module Interface where

<<<<<<< HEAD
=======
import Data.Map (Map)
import Data.Map qualified as Map
import Prob(Prob, runProb, Probability)
import Data.Time

>>>>>>> 6f43335ff100c2c4e1538b9c9eb8fb17a794d3c4
-- Import SDP functions
import SDPCompute

-- Import the SDP module
import GHGCase

timeRunBest :: Int -> Int -> State -> IO ()
timeRunBest decision_step horizon x = do
   startTime <- getCurrentTime
   (a, v) <- return (runBest decision_step horizon x)
   putStrLn $ "(a, v): " ++ (show (a, v))
   endTime <- getCurrentTime
   let diff = diffUTCTime endTime startTime
   putStrLn $ "Execution Time: " ++ (show diff)

runBest :: Int -> Int -> State -> (Action, Val)
runBest = best ghgcase

runmMeas :: Int -> Int -> State -> Val
runmMeas = mMeas ghgcase
