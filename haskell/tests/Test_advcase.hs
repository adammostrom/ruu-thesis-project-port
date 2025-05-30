{-|
Module        : Test_advcase
Description   : QuickCheck property tests for the Climate-Economy SDP Model
Copyright     : (c) Group 12, 2025
License       : MIT

This module implements property-based tests for the climate-economy model using QuickCheck.
The tests verify the correctness of:

* Value Function (val')
  - Base case (empty policy)
  - Non-negativity
  - Monotonicity with policy length

* Backward Induction (bi')
  - Policy sequence length
  - State coverage
  - Policy validity

* Policy Extensions (bestExt', worstExt')
  - Optimality of best vs worst
  - Deterministic behavior
  - Coverage of state space

* Transition Probabilities (next)
  - Probability distribution properties
  - Deterministic behavior
  - State space coverage

* State Measurements (mMeas')
  - Value bounds
  - Measurement validity

Test categories are clearly separated and can be run individually or as a complete suite.
-}
module Test_advcase where

import qualified Data.Map as Map
import AdvancedStates
import SDPCompute(val', bi', bestExt', worstExt', best', mMeas', PolicySeq, SDP)
import Test.QuickCheck
import Prob (Prob, weights, unProb)

-- =====================================================================
-- Arbitrary Instances
-- =====================================================================

{-| Generate arbitrary actions for testing.
    Uses the climate-economy model's action space: MaxEcon, MaxClim, Passive
-}
instance Arbitrary Action where
  arbitrary = elements AdvancedStates.getActions

{-| Generate arbitrary states for testing.
    Uses the climate-economy model's state space combining:
    - Action type
    - Economic level (1..e)
    - Climate level (1..c)
-}
instance Arbitrary State where 
  arbitrary = elements AdvancedStates.getStates

{-| Configuration for the specific climate-economy SDP model under test.
    
    Components:
    * sdpInstance   - The complete SDP model instance
    * sdpGetActions - Available actions in the model
    * sdpNext       - State transition function
    * sdpActions    - State-dependent action availability
    * sdpStates     - Complete state space enumeration
-}
sdpInstance :: SDP State Action
sdpInstance   = advcase
sdpGetActions :: [Action]
sdpGetActions = AdvancedStates.getActions
sdpNext :: Time -> State -> Action -> Prob State
sdpNext       = AdvancedStates.next
sdpActions :: Time -> State -> [Action]
sdpActions    = AdvancedStates.actions
sdpStates :: [State]
sdpStates     = AdvancedStates.getStates

-- =====================================================================
-- Test val'
-- =====================================================================

-- | Test that when the policy sequence is empty, the value function `val'` returns 0.
prop_valEmptyPolicy :: Int -> State -> Property
prop_valEmptyPolicy t x =
  t >= 0 ==> val' advcase t [] x == 0

-- | Test that the value function `val'` always returns a non-negative value for any generated policy sequence.
prop_valNonNegative :: State -> Property
prop_valNonNegative x =
  forAll genValidIntZero $ \t ->
  forAll genValidInt $ \n ->
    let policySeq = bi' advcase t n
    in val' advcase t policySeq x >= 0

-- | Test that a longer policy sequence is at least as good as the original sequence (geq).
prop_longerPolicyBetterOrEqual ::State -> Property
prop_longerPolicyBetterOrEqual x =
  forAll genValidIntZero $ \t ->
  forAll genValidInt $ \n ->
    let ps = bi' advcase t n
    in not (null ps) ==>
      let v1 = val' advcase t ps x
          v2 = val' advcase t (ps ++ [last ps]) x
      in v2 >= v1

-- =====================================================================
-- Test bi'
-- =====================================================================

-- | Test that the length of the policy sequence generated by `bi'` matches the specified horizon `n`.
prop_biLength :: Property
prop_biLength =
  forAll genValidInt $ \n ->
  forAll genValidInt $ \t ->
  length (bi' advcase t n) == n

-- | Test that `bi'` returns an empty policy sequence when n <= 0, regardless of timestep (value of t).
prop_biEmptyPolicy :: Property
prop_biEmptyPolicy =
  forAll genValidIntZero $ \t ->
    bi' advcase t 0 == []

-- | Test that the sequence produced by `bi'` has exactly n elements.
prop_biCorrectLength ::  Property
prop_biCorrectLength =
  forAll genValidIntZero $ \t ->
  forAll genValidInt $ \n ->
    n == length (bi' advcase t n )

-- | Test that every element of the sequence produced by `bi'` is a valid policy.
prop_biCorrectElements :: Property
prop_biCorrectElements =
  forAll genValidIntZero $ \t ->
  forAll genValidInt $ \n ->
    let ps = bi' advcase t n
        allStates = sdpStates 
    in all (\p -> all (`Map.member` p) allStates) ps

-- =====================================================================
-- Test bestExt', worstExt'
-- =====================================================================

-- | Test that the length of the list returned by `bestExt'` matches the number of states.
prop_bestExtWorstCorrectLength :: Property
prop_bestExtWorstCorrectLength =
  forAll genValidInt $ \t ->
  forAll genValidInt $ \n ->
    let ps = bi' advcase t n
        allStates = sdpStates 
    in length (bestExt' advcase t ps) == length allStates

-- | Test that the policy from `bestExt'` is "better" (higher val') than that of `worstExt'`.
prop_bestExtGEQworst :: State -> Property
prop_bestExtGEQworst s =
  forAll genValidInt $ \n ->
  forAll genValidInt $ \t ->
    let ps = bi' advcase t n
        bestPol = bestExt' advcase t ps
        worstPol = worstExt' advcase t ps
        bestVal = val' advcase t (bestPol : ps) s
        worstVal = val' advcase t (worstPol : ps) s
    in bestVal >= worstVal

-- | Test that applying `bestExt'` twice produces the same result.
prop_optExtDeterministic :: Property
prop_optExtDeterministic =
  forAll genValidInt $ \t ->
  forAll genValidInt $ \n ->
    let ps = bi' advcase t n
        p1 = bestExt' advcase t ps
        p2 = bestExt' advcase t ps
    in p1 == p2

-- | Test that applying `bestExt'` twice produces the same value for the policy sequence.
prop_bestExtPolicyIdempotent :: State -> Property
prop_bestExtPolicyIdempotent s =
  forAll genValidInt $ \t ->
  forAll genValidInt $ \n ->
    let ps = bi' advcase t n
        bestP = bestExt' advcase t ps
        newPs = bestP : ps
        v1 = val' advcase t newPs s
        v2 = val' advcase t newPs s
    in v1 == v2

-- =====================================================================
-- Test best'
-- =====================================================================

-- | Test that the action returned by `best'` is one of the possible actions in that state at time t.
prop_bestActionIsValid :: State -> Property
prop_bestActionIsValid s =
  forAll genValidInt $ \t ->
  forAll genValidInt $ \n ->
    let (a, _) = best' advcase t n s
        validActions = AdvancedStates.actions t s
    in a `elem` validActions

-- | Test that `best'` returns the same value as computing `val'` with the returned action and policy.
prop_bestValMatchesVal :: State -> Property
prop_bestValMatchesVal s =
  forAll genValidInt $ \t ->
  forAll genValidInt $ \n ->
    let (a, v) = best' advcase t n s
        ps = bi' advcase (t + 1) (n - 1)
        p = Map.singleton s a
        v' = val' advcase t (p : ps) s
    in v === v'

-- | Test that the value returned by `best'` is greater than or equal to any other possible single-action value.
prop_bestValIsOptimal :: State -> Property
prop_bestValIsOptimal s =
  forAll genValidInt $ \t ->
  forAll genValidInt $ \n ->
    let (_, vBest) = best' advcase t n s
        ps = bi' advcase (t + 1) (n - 1)
        otherActions = AdvancedStates.actions t s
        altVals = [val' advcase t (Map.singleton s a' : ps) s | a' <- otherActions]
    in all (<= vBest) altVals

-- =====================================================================
-- Test meas
-- =====================================================================

-- | Test that the value returned from `mMeas'` is never larger than 1.0.
prop_valueLTOne :: State -> Property
prop_valueLTOne x =
  forAll genValidInt $ \t ->
  forAll genValidInt $ \n ->
    mMeas' advcase t n x <= 1.0

-- =====================================================================
-- Test next
-- =====================================================================

-- | Test that probabilities returned by `next` sum to 1.
prop_sumEqualToOne :: State -> Property
prop_sumEqualToOne x = 
  forAll genValidInt $ \t ->
  forAll (genValidAction t x) $ \s ->
    sum (Prob.weights (AdvancedStates.next t x s)) >= 0.99 &&
    sum (Prob.weights (AdvancedStates.next t x s)) <= 1.01

-- | Test that `next` is deterministic (returns the same probability distribution given the same inputs).
prop_nextDeterministic :: State -> Property
prop_nextDeterministic x = 
  forAll genValidInt $ \t ->
  forAll (genValidAction t x) $ \s ->
    let d0 = unzip $ unProb (AdvancedStates.next t x s)
        d1 = unzip $ unProb (AdvancedStates.next t x s)
    in fst d0 == fst d1 && snd d0 == snd d1

-- =====================================================================
-- Test Runners
-- =====================================================================

-- | Run tests for `val'`.
testVal :: IO ()
testVal = do
  quickCheck prop_valEmptyPolicy
  quickCheck prop_valNonNegative
  quickCheck prop_longerPolicyBetterOrEqual

-- | Run tests for `bi'`.
testBi :: IO ()
testBi = do
  quickCheck prop_biLength
  quickCheck prop_biEmptyPolicy
  quickCheck prop_biCorrectLength
  quickCheck prop_biCorrectElements

-- | Run tests for `bestExt'` and `worstExt'`.
testBestExt :: IO ()
testBestExt = do
  quickCheck prop_bestExtGEQworst
  quickCheck prop_bestExtWorstCorrectLength
  quickCheck prop_bestExtPolicyIdempotent
  quickCheck prop_optExtDeterministic

-- | Run tests for `best'`.
testBest :: IO ()
testBest = do
  quickCheck prop_bestActionIsValid
  quickCheck prop_bestValMatchesVal
  quickCheck prop_bestValIsOptimal

-- | Run tests for `next`.
testNext :: IO ()
testNext = do
  quickCheck prop_sumEqualToOne
  quickCheck prop_nextDeterministic

-- | Run tests for `mMeas'`.
testmMeas :: IO ()
testmMeas = do
  quickCheck prop_valueLTOne

-- | Run all tests.
testAll :: IO ()
testAll = do
  testVal
  testBi
  testBestExt
  testBest
  testNext
  testmMeas

-- =====================================================================
-- Utility Functions
-- =====================================================================

-- | Run `bi'` with the given parameters.
runBi :: Int -> Int -> PolicySeq State Action
runBi t n = bi' advcase t n

-- =====================================================================
-- Test Generators
-- =====================================================================

{-| Generate valid test integers in range [1,7].
    Used for testing time steps and horizons.
-}
genValidInt :: Gen Int
genValidInt = choose (1, 7)

{-| Generate valid test integers in range [0,7].
    Includes 0 for testing boundary conditions.
-}
genValidIntZero :: Gen Int
genValidIntZero = choose (0, 7)

{-| Generate valid actions for a given state and time.
    
    Parameters:
    * t - Time step
    * x - Current state
    
    Returns: A generator for valid actions in that state
-}
genValidAction :: Int -> State -> Gen Action
genValidAction t x = elements (AdvancedStates.actions t x)

main :: IO ()
main = do
    putStrLn "Running all advanced tests..."
    testAll