module Test_matterMost where


import Theory hiding (State, Action)
import Specification
import Test.QuickCheck


instance Arbitrary State where
    arbitrary = elements [DHU, DHC, DLU, DLC, SHU, SHC, SLU, SLC] 

instance Arbitrary Action where
    arbitrary = elements [Start, Delay, Unit] 

----------------- Unit Tests ---------------------------------------------------
{-
    Idris Results:

    *Responsibility> :exec best 0 1 DHU
    "Horizon, best, value:  1,  Delay,  0.468"
-}
unit_best :: Bool
unit_best = 
    let result = bestWrap 0 1 DHU
    in fst result == Delay && snd result >= 0.467 && snd result <= 0.469


----------------- Property Tests -----------------------------------------------
prop_best_geq_worst :: State -> Property
prop_best_geq_worst s =
  forAll (choose (1, 5)) $ \h ->
    let ps = biWrap 0 h
        bestPol = bestExtWrap 0 ps
        worstPol = worstExtWrap 0 ps

        bestVal = valWrap (bestPol : ps) s
        worstVal = valWrap (worstPol : ps) s

    in bestVal >= worstVal

-- Val returns positive values
prop_val_non_negative ::  State -> Property
prop_val_non_negative s = forAll (choose (1, 5)) $ \h -> 
    let ps = biWrap 0 h
        v  = valWrap 0 ps s 
    in v >= 0

-- best functions returns valid actions for that state
prop_best_gives_valid_action :: State -> Property
prop_best_gives_valid_action s = 
    forAll (choose (1, 5)) $ \h ->
        let (a,_) = bestWrap 0 h s
            validActions = actions h s
        in a `elem` validActions
{- 
prop_best_worst :: Int -> Int -> Bool
prop_best_worst t n =
    let ps = bi t n
        best = [bestExtWrap t ps]
        worst = [worstExtWrap t ps]
    in val t ps (fst best) >= val t ps (fst worst)
 -}





-- A function to run any property
run_tests :: Testable prop => String -> prop -> IO ()
run_tests name test = do
    putStrLn $ "Running test: " ++ name
    quickCheck test

-- Concrete runner for your specific prop
run_prop_val_non_negative :: IO ()
run_prop_val_non_negative = run_tests "prop_val_non_negative" prop_val_non_negative




main :: IO()
main = do
    quickCheck prop_val_non_negative
    quickCheck prop_best_gives_valid_action






{- 
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
    runQuickCheckTests -}