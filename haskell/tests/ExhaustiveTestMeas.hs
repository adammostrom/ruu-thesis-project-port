-- \|
-- Module      : ExhaustiveTestMeas
-- Description : This module provides functionality for testing the `mMeas` function
--               using test data from a file and comparing the computed results from the original
--               Idris given all states and all permutations of the timestep and time horizon between 0 and 7.
--               The sript that generated the result.csv file from the Idris program can be found in: python/src/utils
--
-- Usage:
-- - Prepare a test data file with the required format: <State> <timestep> <horizon> <Val>
--   The first 3 paramters being the input, the last is the value returned from the program.
-- - Run the program to execute the tests and generate a results file.
-- - Review the results file to verify the correctness of the `best` function implementation.
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Maybe (isJust, mapMaybe)
import GHGCase
import Prob (Prob, Probability, runProb, unProb, weights)
import SDPCompute
import Test.QuickCheck
import Text.Printf (printf)
import Text.Read (readMaybe)

instance Arbitrary Action where
  arbitrary = elements [Start, Delay, Unit]

instance Arbitrary State where
  arbitrary = elements GHGCase.getStates

-- | Computes the measurement value for a given state, time, and number of iterations. Returns 0 if the number of iterations is 0.
testMeas :: Int -> Int -> State -> Double
testMeas t n x
  | n == 0 = 0
  | otherwise = mMeas ghgcase t n x

-- | Parses a single line of test data into a tuple containing a `State`, two integers, and a `Double`. Returns `Nothing` if the line cannot be parsed.
parseRowMeas :: String -> Maybe (State, Int, Int, Double)
parseRowMeas line =
  case words line of
    [stateStr, xStr, yStr, valueStr] -> do
      state <- readMaybe stateStr :: Maybe State
      x <- readMaybe xStr :: Maybe Int
      y <- readMaybe yStr :: Maybe Int
      value <- readMaybe valueStr :: Maybe Double
      Just (state, x, y, value)
    _ -> Nothing

-- | Reads a file containing test data, parses each line using `parseRowMeas`, and returns a list of parsed tuples.
readTestData :: FilePath -> IO [(State, Int, Int, Double)]
readTestData file = do
  content <- readFile file
  let parsedData = mapMaybe parseRowMeas (lines content)
  return parsedData

-- | Compares the computed value of `testMeas` with an expected value for a given state, time, and number of iterations.
compareTestResults :: Int -> Int -> State -> Double -> IO String
compareTestResults t n x expectedValue = do
  let computedValue = testMeas t n x
  if abs (computedValue - expectedValue) < 1e-7
    then
      return $
        "Test Passed for "
          ++ show x
          ++ " "
          ++ show t
          ++ ","
          ++ show n
          ++ "| Expected: "
          ++ show expectedValue
          ++ "| Got: "
          ++ show computedValue
    else
      return $
        "Test Failed for "
          ++ show x
          ++ " "
          ++ show t
          ++ ","
          ++ show n
          ++ "| Expected: "
          ++ show expectedValue
          ++ "| Got: "
          ++ show computedValue

-- | Reads test data from an input file, runs tests for each data point, and writes the results to an output file.
runTests :: FilePath -> FilePath -> IO ()
runTests inputFile outputFile = do
  testData <- readTestData inputFile
  results <- mapM (\(x, t, n, v) -> compareTestResults t n x v) testData
  writeFile outputFile (unlines results)

-- Main entry to run the tests
main :: IO ()
main = do
  runTests
    "haskell/tests/mMeas_test_data.txt"
    "haskell/tests/mMemas_test_results.txt"

-- | A wrapper function for `mMeas` with the `ghgcase` parameter pre-applied.
runMeas :: Int -> Int -> State -> Double
runMeas = mMeas ghgcase