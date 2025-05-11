module Testing_AdvancedStates where
import Data.Map (Map)
import Data.Map qualified as Map
import SDPCompute(bi', best', mMeas', Policy, PolicySeq)
import AdvancedStates(next, reward, actions, states, advcase, State(..), Action(..))
import Prob(Prob, runProb, Probability)
import Text.Printf (printf)

pol :: Policy State Action
pol = Map.fromList [(State Passive 4 2, MaxClim)]

pol2 :: Policy State Action
pol2 = Map.fromList [(State Passive 5 5, Passive)]

pol3 :: Policy State Action
pol3 = Map.fromList [(State Passive 3 3, MaxEcon)]

polseq :: PolicySeq State Action
polseq = [pol, pol2, pol3]

-- Helper function to format floating point numbers
formatFloat :: Double -> String
formatFloat x = printf "%.3f" x

-- Helper function to format probability distributions
formatProb :: [(State, Double)] -> [(State, String)]
formatProb = map (\(s, p) -> (s, formatFloat p))

-- Comparing mMeas values to those of the article, testing.
-- TODO Check if they are correct
main :: IO ()
main = do
    -- Test 1: States at time 0
    putStrLn "States at time step 0:"
    print $ states 0

    -- Test 2: Actions at time 0 for state (D,1,1)
    putStrLn "\nActions at time step 0, state (D,1,1):"
    print $ actions 0 (State Passive 1 1)

    -- Test 3: Next state distribution with formatted probabilities
    putStrLn "\nNext state distribution at time step 0, state (D,1,1), action MaxEcon:"
    print $ formatProb $ runProb $ next 0 (State MaxClim 1 1) MaxEcon

    -- Test 4: Reward calculation with formatted value
    putStrLn "\nReward at time step 0, state (D,1,1), action MaxEcon, next state (D,5,5):"
    putStrLn $ formatFloat $ reward 0 (State Passive 1 1) MaxEcon (State MaxEcon 5 5)

    -- Test 5: Optimal policy sequence
    putStrLn "\nOptimal policy sequence at time step 0, steps=1:"
    print $ bi' advcase 0 1

    -- Test 6: Best action
    putStrLn "\nBest action at time step 0, state (D,1,1):"
    print $ maybe "No action found" show $ Map.lookup (State Passive 1 1) (head $ bi' advcase 0 1)

{-     -- Test 7: Worst case value
    putStrLn "\nWorst case value at time step 0, state (D,1,1):"
    print $ worst' advcase 0 1 (State Passive 1 1) -}

    -- Test 8: Measurements with formatted values
    putStrLn "\nMeasurements at time step 0, state (D,1,1), numSteps 7:"
    putStrLn $ formatFloat $ mMeas' advcase 0 7 (State Passive 1 1)