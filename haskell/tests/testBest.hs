{-# LANGUAGE ScopedTypeVariables #-}

import Test.QuickCheck
import Data.Maybe (isJust, mapMaybe)
import Text.Read (readMaybe)

-- Assuming `best` is defined in a module called `BestModule`
import CoreComputation (State(..), best, Action(..) )

-- Arbitrary instance for Action
instance Arbitrary Action where
    arbitrary = elements [Start, Delay, Unit]  -- Randomly pick one of these constructors

-- Arbitrary instance for State
instance Arbitrary State where
    arbitrary = elements [DHU, DHC, DLU, DLC, SHU, SHC, SLU, SLC]  -- Randomly pick one of these constructors

    shrink _ = []


extractFst :: (a, b, c) -> a
extractFst (a, b, c) = a

extractSnd :: (a, b, c) -> b
extractSnd (a, b, c) = b

extractTrd :: (a, b, c) -> c
extractTrd (a, b, c) = c


--Test Horizon
testBestHorizon :: State -> Int -> Int -> Int
testBestHorizon s x y = extractFst $ best x y s

testBestAction :: State -> Int -> Int -> Action
testBestAction s x y = extractSnd $ best x y s

testBestValue :: State -> Int -> Int -> Double
testBestValue s x y = extractTrd $ best x y s

testBest :: State -> Int -> Int -> (Int, Action, Double) -> Bool
testBest s x y (a, b, c) 
    |  testBestHorizon s x y == extractFst (a, b, c)
    && testBestAction s x y  == extractSnd (a, b, c) 
    && testBestValue s x y   == extractTrd (a, b, c)   = True
    | otherwise                                        = False

-- First Unit Test
{- 
prop_best_0_1_DHU :: Int -> Int -> State -> Bool
prop_best_0_1_DHU x y state =
    let result = best 0 1 DHU
    in fst result == Delay && snd result == 0.4679999999997
 -}
{- -- Property: best should return a value between 0 and 1
prop_bestInRange :: Int -> Int -> ValTest.State -> Bool
prop_bestInRange x y state =
    let result = best x y state
    in result >= 0 && result <= 1

-- Property: best should be deterministic (same input -> same output)
prop_bestDeterministic :: Int -> Int -> ValTest.State -> Bool
prop_bestDeterministic x y state =
    best x y state == best x y state
-}

{- -- Run tests
main :: IO ()
main = do
    #quickCheck prop_best_0_1_DHU -}


-- Function to parse a line from the text file

-- Function to parse a line from the text file
-- Updated parseRow function to return the correct structure
parseRow :: String -> Maybe (State, Int, Int, (Action, Double))
parseRow line =
    case words line of
        [stateStr, xStr, yStr, horizonStr, actionStr, valueStr] -> do
            state <- readMaybe stateStr :: Maybe State  -- Explicit type annotation
            x <- readMaybe xStr :: Maybe Int
            y <- readMaybe yStr :: Maybe Int
            horizon <- readMaybe horizonStr :: Maybe Int
            action <- readMaybe actionStr :: Maybe Action  -- Explicit type annotation
            value <- readMaybe valueStr :: Maybe Double
            Just (state, x, y, (action, value))
        _ -> Nothing

-- Reads the text file and parses each line
readTestData :: FilePath -> IO [(State, Int, Int, (Action, Double))]
readTestData file = do
    content <- readFile file
    let parsedData = mapMaybe parseRow (lines content)
    return parsedData

-- Function to compare the best values and write results to a file
compareTestResults :: State -> Int -> Int -> (Action, Double) -> IO String
compareTestResults s x y (expectedAction, expectedValue) = do
    let computedAction = testBestAction s x y
    let computedValue = testBestValue s x y
    if computedAction == expectedAction && abs (computedValue - expectedValue) < 1e-6
        then return $ "Test Passed for " ++ show s ++ " " ++ show x ++ "," ++ show y
        else return $ "Test Failed for " ++ show s ++ " " ++ show x ++ "," ++ show y
                    ++ ". Expected: " ++ show (expectedAction, expectedValue)
                    ++ ", Got: " ++ show (computedAction, computedValue)

-- Function to run all tests and save the result to a file
runTests :: FilePath -> FilePath -> IO ()
runTests inputFile outputFile = do
    testData <- readTestData inputFile
    results <- mapM (\(s, x, y, (a, v)) -> compareTestResults s x y (a, v)) testData
    writeFile outputFile (unlines results)

-- Main entry to run the tests
main :: IO ()
main = do
    runTests "test_data.txt" "test_results.txt"

