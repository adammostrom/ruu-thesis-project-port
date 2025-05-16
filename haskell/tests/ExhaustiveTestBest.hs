-- \|
-- Module      : ExhaustiveTestBest
-- Description : This module provides functionality to test the `best` function from the GHGCase module
--               using test data from a file and comparing the computed results from the original
--               Idris given all states and all permutations of the timestep and time horizon between 0 and 7.
--               The sript that generated the result.csv file from the Idris program can be found in: python/src/utils
-- Usage:
-- - Prepare a test data file with the required format: <State> <timestep> <horizon> <timestep> <Action> <Val>
--   The first 3 paramters being the input, the latter 3 the output from the program.
-- - Run the program to execute the tests and generate a results file.
-- - Review the results file to verify the correctness of the `best` function implementation.
--
-- Notes:
-- - The `compareTestResults` function uses a tolerance of 1e-6 for comparing floating-point values.
-- - Ensure that the input test data file is correctly formatted to avoid parsing errors.
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Maybe (isJust, mapMaybe)
import GHGCase
import Prob (Prob, Probability, runProb)
import SDPCompute
import Test.QuickCheck
import Text.Printf (printf)
import Text.Read (readMaybe)

instance Arbitrary Action where
  arbitrary = elements [Start, Delay, Unit]

instance Arbitrary State where
  arbitrary = elements GHGCase.getStates

-- | Computes the best action for a given state and parameters using the `best` function.
testBestAction :: State -> Int -> Int -> Action
testBestAction s x y = fst $ best ghgcase x y s

-- | Computes the best value for a given state and parameters using the `best` function.
testBestValue :: State -> Int -> Int -> Double
testBestValue s x y = snd $ best ghgcase x y s

-- | Verifies if the computed best action and value match the expected results.
testBest :: State -> Int -> Int -> (Action, Double) -> Bool
testBest s x y (a, v)
  | testBestAction s x y == a
      && testBestValue s x y == v =
      True
  | otherwise = False

-- | Parses a single line of test data from a text file into a structured format.
parseRow :: String -> Maybe (State, Int, Int, (Action, Double))
parseRow line =
  case words line of
    [stateStr, xStr, yStr, horizonStr, actionStr, valueStr] -> do
      state <- readMaybe stateStr :: Maybe State
      x <- readMaybe xStr :: Maybe Int
      y <- readMaybe yStr :: Maybe Int
      horizon <- readMaybe horizonStr :: Maybe Int
      action <- readMaybe actionStr :: Maybe Action
      value <- readMaybe valueStr :: Maybe Double
      Just (state, x, y, (action, value))
    _ -> Nothing

-- | Reads a text file containing test data and parses each line into structured test cases.
readTestData :: FilePath -> IO [(State, Int, Int, (Action, Double))]
readTestData file = do
  content <- readFile file
  let parsedData = mapMaybe parseRow (lines content)
  return parsedData

-- | Compares the computed best action and value with the expected results and generates a test result message.
compareTestResults :: State -> Int -> Int -> a -> IO String
compareTestResults s x y (expectedAction, expectedValue) = do
  let computedAction = testBestAction s x y
  let computedValue = testBestValue s x y
  if computedAction == expectedAction && abs (computedValue - expectedValue) < 1e-6
    then
      return $
        "Test Passed for "
          ++ show s
          ++ " "
          ++ show x
          ++ ","
          ++ show y
          ++ "| Expected: "
          ++ show (expectedAction, expectedValue)
          ++ "| Got: "
          ++ show (computedAction, computedValue)
    else
      return $
        "Test Failed for "
          ++ show s
          ++ " "
          ++ show x
          ++ ","
          ++ show y
          ++ "| Expected: "
          ++ show (expectedAction, expectedValue)
          ++ "| Got: "
          ++ show (computedAction, computedValue)

-- | Runs all tests by reading test data from an input file, comparing results, and saving the test outcomes to an output file.
runTests :: FilePath -> FilePath -> IO ()
runTests inputFile outputFile = do
  testData <- readTestData inputFile
  results <- mapM (\(s, x, y, (a, v)) -> compareTestResults s x y (a, v)) testData
  writeFile outputFile (unlines results)

main :: IO ()
main = do
  runTests
    "haskell/tests/best_test_data.txt"
    "haskell/tests/best_test_results.txt"
