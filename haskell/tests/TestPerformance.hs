-- File: TestPerformance.hs
-- This file is used to test the performance of the model.

module TestPerformance where

import SDPCompute
import GHGCase
import Prob(Prob, runProb, Probability)
import System.CPUTime
import Text.Printf (printf)

import Control.Monad (forM_)


test_best :: Int -> Int -> State -> (Action, Val)
test_best t n x = best ghgcase t n DHU


main :: IO ()
main = do
    putStrLn ("Time measurement is currently available for function -best- ")
    forM_ [1..12] $ \n -> do
        start <- getCPUTime

        let (a, v) = best' ghgcase 0 n DHU -- adjust the function
        a `seq` v `seq` return ()  -- force both parts of the tuple

        end <- getCPUTime
        let diff = fromIntegral (end - start) / (10^12) :: Double
        printf ("Step: " ++ show n ++ " Time: %.6f seconds\n") diff