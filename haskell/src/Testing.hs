module Testing where
import Data.Map (Map)
import Data.Map qualified as Map
import SDPCompute
import GHGCase
import Prob(Prob, runProb, Probability)
import Control.DeepSeq (NFData, deepseq)
import System.CPUTime (getCPUTime)
import Text.Printf (printf)

pol :: Policy State Action
pol = Map.fromList [(DHU, Delay)]

pol2 :: Policy State Action
pol2 = Map.fromList [(DHC, Start)]

pol3 :: Policy State Action
pol3 = Map.fromList [(DHU, Delay)]

polseq :: PolicySeq State Action
polseq = [pol, pol2, pol3]

simplePol :: PolicySeq State Action
simplePol = [Map.fromList [(DHU, Start), (DHC, Delay)]]

biTest :: PolicySeq State Action
biTest = bi ghgcase 0 1

-- Helper function for timing measurements
timeTest :: (NFData a) => String -> IO a -> IO a
timeTest name action = do
    start <- getCPUTime
    result <- action
    result `deepseq` return ()  -- Force full evaluation
    end <- getCPUTime
    let diff = fromIntegral (end - start) / (10^12)
    printf "%s took %0.3f seconds\n" name (diff :: Double)
    return result

-- Compare original and memoized versions
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
    
    -- Test case 4: bi vs bi'
    -- putStrLn "\nTest case 4: Comparing bi and bi' (ghgcase 0 7)"
    -- pol1 <- timeTest "bi  " $ return $ bi ghgcase 0 7
    -- pol2 <- timeTest "bi' " $ return $ bi' ghgcase 0 7
    -- putStrLn $ "Policy sequences equal: " ++ show (pol1 == pol2)
    
    -- Test case 5: val vs val'
    putStrLn "\nTest case 5: Comparing val and val' with test policy"
    v1 <- timeTest "val  " $ return $ val ghgcase 0 [myPol] DHU
    v2 <- timeTest "val' " $ return $ val' ghgcase 0 [myPol] DHU
    printf "Values: %f vs %f\n" v1 v2

    -- Test case 6: mMeas vs mMeas'
    putStrLn "\nTest case 6: ghgcase 3 10 DHU"
    val6 <- timeTest "mMeas " $ return $ mMeas ghgcase 3 10 DHU
    val6' <- timeTest "mMeas'" $ return $ mMeas' ghgcase 3 10 DHU
    printf "Values: %f vs %f\n" val6 val6'

-- PJ: test policy
myPol :: Policy State Action
myPol = constPol Start

constPol :: Action -> Policy State Action
constPol a = Map.fromList
  [ (DHU,a)
  , (DHC,a)
  , (DLU,a)
  , (DLC,a)
  , (SHU,a) 
  , (SHC,a)
  , (SLU,a)
  , (SLC,a)
  ]

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