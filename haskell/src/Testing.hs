module Testing where
import Data.Map (Map)
import Data.Map qualified as Map
import SDPCompute
import GHGCase
import Prob(Prob, runProb, Probability)
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

-- Comparing mMeas values to those of the article, testing.
-- TODO Check if they are correct
main :: IO ()
main = do
    putStrLn "Testing mMeas function..."
    print $ mMeas ghgcase 0 4 DHU
    print $ mMeas ghgcase 0 6 SLC
    print $ mMeas ghgcase 0 7 DHU
    print $ mMeas ghgcase 1 7 DHU
    print $ mMeas ghgcase 3 7 DHU

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
