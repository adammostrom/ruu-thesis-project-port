import Test.QuickCheck
import Data.Maybe (isJust, mapMaybe)
import Text.Read (readMaybe)
import Text.Printf(printf)
import Debug.Trace

-- Assuming `best` is defined in a module called `BestModule`
import CoreComputation (Prob, State(..), best, Action(..), next, unwrapProbState)

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

-- Function to check if the probabilities sum to 1
validProb :: Prob State -> Bool
validProb prob = abs (sum (map snd (unwrapProbState prob)) - 1.0) < 1e-6  -- Allow small floating-point errors

-- Property to test if probabilities sum to 1 for each state-action pair
prop_probSum :: Int -> State -> Action -> Property
prop_probSum t x y = validProb (next t x y) ==> validProb (next t x y)

-- Example: Running the test
main :: IO ()
main = quickCheck (prop_probSum 0 DHU Start)
