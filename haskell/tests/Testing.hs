-- |
-- Module      : Testing
-- Description : Performance testing playground for GHGCase model
-- Copyright   : (c) Group 12, 2025
-- License     : MIT
--
-- This module provides an experimental playground for comparing performance
-- between memoized and non-memoized versions of SDP computations. Features:
--
-- Test Cases:
-- * Timing comparisons between:
--   - mMeas vs mMeas' (state measurements)
--   - bi vs bi' (backward induction)
--   - val vs val' (value function)
--
-- Performance Utilities:
-- * timeTest - CPU time measurement helper
-- * Various test policies and sequences
-- * Long-horizon computation examples
--
-- Sample Configurations:
-- * Simple constant policies
-- * Multi-step policy sequences
-- * Performance tests with increasing horizons
--
-- Note: This is not a formal test suite - it's a playground for
-- experimenting with the GHGCase model and comparing algorithm performance.
-- For formal testing, see the tests/ directory.
module Testing where

import Control.DeepSeq (NFData, deepseq)
import Data.Map qualified as Map
import GHGCase
import SDPCompute
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

-- | Basic policy that always delays.
--    Used as a baseline for performance testing.
pol :: Policy State Action
pol = Map.fromList [(DHU, Delay)]

-- | Test policy that starts at DHC.
--    Used to test different initial conditions.
pol2 :: Policy State Action
pol2 = Map.fromList [(DHC, Start)]

pol3 :: Policy State Action
pol3 = Map.fromList [(DHU, Delay)]

-- | Test policy sequence combining multiple simple policies.
--    Used for testing multi-step scenarios.
polseq :: PolicySeq State Action
polseq = [pol, pol2, pol3]

-- | Simple two-state policy for basic tests.
--    Tests both Start and Delay actions.
simplePol :: PolicySeq State Action
simplePol = [Map.fromList [(DHU, Start), (DHC, Delay)]]

biTest :: PolicySeq State Action
biTest = bi ghgcase 0 1

-- | Helper function for measuring CPU time of computations.
--
--    Parameters:
--    * name   - Description of the computation being timed
--    * action - The IO action to measure
--
--    Returns: Result of the action and prints timing information
--
--    Note: Uses deepseq to ensure full evaluation
timeTest :: (NFData a) => String -> IO a -> IO a
timeTest name action = do
  start <- getCPUTime
  result <- action
  result `deepseq` return () -- Force full evaluation
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10 ** 12)
  printf "%s took %0.3f seconds\n" name (diff :: Double)
  return result

-- | Main function running all performance comparisons.
--    Compares memoized vs non-memoized versions of:
--    * mMeas vs mMeas'
--    * bi vs bi'
--    * val vs val'
--
--    Outputs timing information and value comparisons.
main :: IO ()
main = do
  putStrLn "\nComparing original vs memoized mMeas function..."

  -- Test case 1: mMeas vs mMeas'
  putStrLn "\nTest case 1: ghgcase 0 4 DHU"
  val1 <- timeTest "mMeas " $ return $ mMeas ghgcase 0 4 DHU
  val1' <- timeTest "mMeas'" $ return $ mMeas' ghgcase 0 4 DHU
  printf "Values: %f vs %f\n" val1 val1'

  -- Test case 2: mMeas vs mMeas'
  putStrLn "\nTest case 2: ghgcase 0 6 SLC"
  val2 <- timeTest "mMeas " $ return $ mMeas ghgcase 0 6 SLC
  val2' <- timeTest "mMeas'" $ return $ mMeas' ghgcase 0 6 SLC
  printf "Values: %f vs %f\n" val2 val2'

  -- Test case 3: mMeas vs mMeas'
  putStrLn "\nTest case 3: ghgcase 0 7 DHU"
  val3 <- timeTest "mMeas " $ return $ mMeas ghgcase 0 7 DHU
  val3' <- timeTest "mMeas'" $ return $ mMeas' ghgcase 0 7 DHU
  printf "Values: %f vs %f\n" val3 val3'

{-   -- Test case 4: bi vs bi'
  putStrLn "\nTest case 4: Comparing bi and bi' (ghgcase 0 7)"
  pol1 <- timeTest "bi  " $ return $ bi ghgcase 0 7
  --pol2 <- timeTest "bi' " $ return $ bi' ghgcase 0 7
  putStrLn $ "Policy sequences equal: " ++ show (pol1 == pol2) -}

  -- Test case 5: val vs val'
  putStrLn "\nTest case 5: Comparing val and val' with test policy"
  v1 <- timeTest "val  " $ return $ val ghgcase 0 [myPol] DHU
  v2 <- timeTest "val' " $ return $ val' ghgcase 0 [myPol] DHU
  printf "Values: %f vs %f\n" v1 v2

  -- Test case 6: mMeas vs mMeas'
  putStrLn "\nTest case 6: ghgcase 2 35 DHU"
  -- val6 <- timeTest "mMeas " $ return $ mMeas ghgcase 2 12 DHU
  val6' <- timeTest "mMeas'" $ return $ bi' ghgcase 0 35
  mapM_ print val6'

  -- Test case 6: mMeas vs mMeas'
  putStrLn "\nTest case 6: ghgcase 2 70 DHU (2 135)"
  -- val6 <- timeTest "mMeas " $ return $ mMeas ghgcase 2 12 DHU
  val7' <- timeTest "mMeas'" $ return $ bi' ghgcase 0 70
  val8' <- timeTest "mMeas'" $ return $ bi' ghgcase 0 140

  mapM_ print val7'
  mapM_ print val8'

-- PJ: test policy
myPol :: Policy State Action
myPol = constPol Start

-- | Create a constant policy that applies the same action to all states.
--
--    Parameters:
--    * a - The action to apply everywhere
--
--    Returns: A policy mapping all states to action a
constPol :: Action -> Policy State Action
constPol a =
  Map.fromList
    [ (DHU, a),
      (DHC, a),
      (DLU, a),
      (DLC, a),
      (SHU, a),
      (SHC, a),
      (SLU, a),
      (SLC, a)
    ]

testVal :: Val
testVal = val ghgcase 0 [myPol] DHU

-- PaJa: some more experiments

-- Compute the optimal policy sequence starting at t=0, with n=7
optps1 :: PolicySeq State Action
optps1 = bi ghgcase 0 7

-- Test helpers for comparing policy sequences
{- testBi :: Int -> IO ()
testBi n = do
    putStrLn $ "\nTesting bi vs bi' with n = " ++ show n
    _ <- timeTest "bi  " $ return $ bi ghgcase 0 n
     _ <- timeTest "bi' " $ return $ bi' ghgcase 0 n
    return () -}

-- Additional test cases
{- testCases :: IO ()
testCases = do
    putStrLn "\nRunning additional test cases..."
    mapM_ testBi [1,2,3,4,5]  -- Test increasing horizons -}