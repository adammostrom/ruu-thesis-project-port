{-# LANGUAGE ScopedTypeVariables #-}
import Test.QuickCheck
import Data.Maybe (isJust, mapMaybe)
import Text.Read (readMaybe)
import Text.Printf(printf)

import SDPCompute
import GHGCase
import Prob(Prob, runProb, Probability)

instance Arbitrary Action where
  arbitrary = elements [Start, Delay, Unit]

instance Arbitrary State where 
  arbitrary = elements GHGCase.getStates


testBestAction :: State -> Int -> Int -> Action
testBestAction s x y = fst $ best ghgcase x y s

testBestValue :: State -> Int -> Int -> Double
testBestValue s x y = snd $ best ghgcase x y s

testBest :: State -> Int -> Int -> (Action, Double) -> Bool
testBest s x y (a, v) 
    | testBestAction s x y  == a
    && testBestValue s x y  == v   = True
    | otherwise                    = False

-- Function to parse a line from the text file
-- Updated parseRow function to return the correct structure
parseRow :: String -> Maybe (State, Int, Int, (Action, Double))
parseRow line =
    case words line of
        [stateStr, xStr, yStr, horizonStr, actionStr, valueStr] -> do
            state   <- readMaybe stateStr   :: Maybe State  
            x       <- readMaybe xStr       :: Maybe Int
            y       <- readMaybe yStr       :: Maybe Int
            horizon <- readMaybe horizonStr :: Maybe Int
            action  <- readMaybe actionStr  :: Maybe Action
            value   <- readMaybe valueStr   :: Maybe Double
            Just (state, x, y, (action, value))
        _ -> Nothing

-- Reads the text file and parses each line
readTestData :: FilePath -> IO [(State, Int, Int, (Action, Double))]
readTestData file = do
    content <- readFile file
    let parsedData = mapMaybe parseRow (lines content)
    return parsedData

-- Function to compare the best values and write results to a file
compareTestResults :: State -> Int -> Int -> a -> IO String
compareTestResults s x y (expectedAction, expectedValue) = do
    let computedAction = testBestAction s x y
    let computedValue = testBestValue s x y
    if computedAction == expectedAction && abs (computedValue - expectedValue) < 1e-6
        
        then return $ "Test Passed for " ++ show s ++ " " ++ show x ++ "," ++ show y
                    ++ "| Expected: " ++ show (expectedAction, expectedValue)
                    ++ "| Got: " ++ show (computedAction, computedValue)
        else return $ "Test Failed for " ++ show s ++ " " ++ show x ++ "," ++ show y
                    ++ "| Expected: " ++ show (expectedAction, expectedValue)
                    ++ "| Got: " ++ show (computedAction, computedValue)

-- Function to run all tests and save the result to a file
runTests :: FilePath -> FilePath -> IO ()
runTests inputFile outputFile = do
    testData <- readTestData inputFile
    results <- mapM (\(s, x, y, (a, v)) -> compareTestResults s x y (a, v)) testData
    writeFile outputFile (unlines results)

-- Main entry to run the tests
main :: IO ()
main = do
    runTests "haskell/tests/best_test_data.txt"
             "haskell/tests/test_results2.txt"

