import Test.QuickCheck
import Data.Maybe (isJust, mapMaybe)
import Text.Read (readMaybe)
import Text.Printf(printf)
import Debug.Trace

{-
This test suite simply covers the immediate functionality of Reward function.
Simply demonstrated by a property test, validating the implementation of the function "reward".
-}


-- Assuming `best` is defined in a module called `BestModule`
import CoreComputation (State(..), best, Action(..), reward )

-- Arbitrary instance for State to generate random values for testing
instance Arbitrary State where
  arbitrary = oneof [return DHU
                    ,return DHC
                    ,return DLU
                    ,return DLC
                    ,return SHU
                    ,return SHC
                    ,return SLU
                    ,return SLC]

-- EXCEPTION. Idris returns: 
{--
    Type mismatch between Val (Type of reward t x _ _)
    and Integer (Expected type)
    Specifically: Type mismatch between
    NonNegDouble and Integer
--}
testReward_SHU_SLC :: Bool
testReward_SHU_SLC = reward 0 SHU Start SLC == 0 -- Idris returns exception, our haskell returns 0. 



-- TODO, Implement more cases?

-- This property checks that if the next state is DHU or SHU, the reward is 1; otherwise, 0.
-- The QuickCheck testing ensures a property that no matter which states we put in as "next_y" 
-- and next_x, if any next_x state is DHU or SHU, we always return 1, else 0. QuickCheck proves this property works.
prop_reward :: State -> State -> Bool
prop_reward next_x next_y = trace ("Testing state: " ++ show next_x) $
    reward 0 next_y Unit next_x == (if next_x == DHU || next_x == SHU then 1 else 0)

-- Utility function to report test results
runTest :: String -> Bool -> IO ()
runTest name result =
    putStrLn $ if result
        then "[PASS] " ++ name 
        else "[FAIL] " ++ name

-- Running unit tests
runUnitTests :: IO ()
runUnitTests = do
    putStrLn "Running Unit Tests..."
    runTest "testReward_DHU" testReward_SHU_SLC

-- Running QuickCheck property tests
runQuickCheckTests :: IO ()
runQuickCheckTests = do
    quickCheck prop_reward

-- Main function that runs both unit tests and QuickCheck tests
main :: IO ()
main = do
    runUnitTests
    runQuickCheckTests