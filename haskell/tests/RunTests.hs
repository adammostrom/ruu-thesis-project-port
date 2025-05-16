-- |
-- Module      : Main
-- Description : Entry point for running test cases for different implementations.
-- Copyright   : (c) GROUP 12, 2023
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX
--
-- Available implementations:
--     - GHGCase: Basic tests for the SDP module.
--     - AdvancedStates: Advanced state tests.
module Main where

import Test_SDP
import Test_advcase

main :: IO ()
main = do
  putStrLn "Available implementations to test: \n 0. GHGCase \n 1. AdvancedStates "
  input <- getLine

  case input of
    "0" -> do ghgcase
    "1" -> do advcase
    _ -> putStrLn "No module for that number"

ghgcase :: IO ()
ghgcase = do
  putStrLn "Running SDP basic tests..."
  putStrLn "=========================="
  Test_SDP.testAll

advcase :: IO ()
advcase = do
  putStrLn "\nRunning advanced state tests..."
  putStrLn "=============================="
  Test_advcase.testAll