-- |
-- Module      : Main
-- Description : Entry point for running test cases for different implementations.
-- Copyright   : (c) GROUP 12, 2023
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX
module Main where

import qualified Test_SDP as TSDP
import qualified Test_advcase as TADV

tests :: [(String, IO ())]
tests =
  [ ("GHGCase - Basic tests for the SDP module", TSDP.testAll)
  , ("AdvancedStates - Advanced state tests", TADV.testAll)
  ]

main :: IO ()
main = do
  putStrLn "Available implementations to test:"
  mapM_ (\(i, (desc, _)) -> putStrLn $ " " ++ show i ++ ". " ++ desc) (zip [0..] tests)

  putStrLn "\nEnter the number of the test to run:"
  input <- getLine

  let maybeIndex = reads input :: [(Int, String)]
  case maybeIndex of
    [(n, _)] | n >= 0 && n < length tests -> snd (tests !! n)
    _ -> putStrLn "Invalid selection. Please enter a valid number."
