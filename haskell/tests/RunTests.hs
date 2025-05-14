module Main where

import Test_SDP
import Test_advcase
import System.Exit (exitFailure, exitSuccess)

main :: IO()
main = do
    putStrLn "Running SDP basic tests..."
    putStrLn "=========================="
    Test_SDP.testAll
    
    putStrLn "\nRunning advanced state tests..."
    putStrLn "=============================="
    Test_advcase.testAll