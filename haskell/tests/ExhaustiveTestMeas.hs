{-# LANGUAGE ScopedTypeVariables #-}
import Test.QuickCheck
import Data.Maybe (isJust, mapMaybe)
import Text.Read (readMaybe)
import Text.Printf(printf)

import GHGCase
import SDPCompute
import Prob (Prob, runProb, Probability, weights, unProb)

instance Arbitrary Action where
  arbitrary = elements [Start, Delay, Unit]

instance Arbitrary State where 
  arbitrary = elements GHGCase.getStates

testMeas :: Int -> Int -> State -> Double
testMeas t n x 
    | n == 0    = 0
    | otherwise = mMeas ghgcase t n x


parseRowMeas :: String -> Maybe (State, Int, Int, Double)
parseRowMeas line =
    case words line of
        [stateStr, xStr, yStr, valueStr] -> do
            state <- readMaybe stateStr :: Maybe State  
            x     <- readMaybe xStr     :: Maybe Int
            y     <- readMaybe yStr     :: Maybe Int
            value <- readMaybe valueStr :: Maybe Double
            Just (state, x, y, value)
        _ -> Nothing

-- Reads the text file and parses each line
readTestData :: FilePath -> IO [(State, Int, Int, Double)]
readTestData file = do
    content <- readFile file
    let parsedData = mapMaybe parseRowMeas (lines content)
    return parsedData


-- Function to compare the best values and write results to a file
compareTestResults :: Int -> Int -> State -> Double -> IO String
compareTestResults t n x expectedValue = do
    let computedValue = testMeas t n x
    if abs (computedValue - expectedValue) < 1e-6
        
        then return $ "Test Passed for " ++ show x ++ " " ++ show t ++ "," ++ show n
                    ++ "| Expected: " ++ show expectedValue
                    ++ "| Got: " ++ show computedValue
        else return $ "Test Failed for " ++ show x ++ " " ++ show t ++ "," ++ show n
                    ++ "| Expected: " ++ show expectedValue
                    ++ "| Got: " ++ show computedValue

-- Function to run all tests and save the result to a file
runTests :: FilePath -> FilePath -> IO ()
runTests inputFile outputFile = do
    testData <- readTestData inputFile
    results <- mapM (\(x, t, n, v) -> compareTestResults t n x v) testData
    writeFile outputFile (unlines results)

-- Main entry to run the tests
main :: IO ()
main = do
    runTests "haskell/tests/mMeas_test_data.txt"
             "haskell/tests/mMemas_test_results.txt"

runMeas :: Int -> Int -> State -> Double
runMeas = mMeas ghgcase