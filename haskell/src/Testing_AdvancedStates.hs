module Testing_AdvancedStates where
import Data.Map (Map)
import Data.Map qualified as Map
import SDPCompute
import AdvancedStates
import Prob(Prob, runProb, Probability)

pol :: Policy State Action
pol = Map.fromList [(State 'D' 4 2, MaxClim)]

pol2 :: Policy State Action
pol2 = Map.fromList [(State 'D' 5 5, Passive)]

pol3 :: Policy State Action
pol3 = Map.fromList [(State 'D' 3 3, MaxEcon)]

polseq :: PolicySeq State Action
polseq = [pol, pol2, pol3]


-- Comparing mMeas values to those of the article, testing.
-- TODO Check if they are correct
main :: IO ()
main = do
    putStrLn "Testing mMeas function..."
    print $ mMeas advcase 0 1 (State 'D' 4 2)
    print $ mMeas advcase 0 1 (State 'D' 5 5)
    print $ mMeas advcase 0 1 (State 'D' 3 3)

{-
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

testVal = val advcase 0 [myPol] DHU 

-- PaJa: some more experiments

-- Compute the optimal policy sequence starting at t=0, with n=7
optps1 :: PolicySeq State Action
optps1 = bi advcase 0 7

-} 