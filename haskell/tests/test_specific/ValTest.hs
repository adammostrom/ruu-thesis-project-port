import Test.QuickCheck
import Data.Maybe (isJust, mapMaybe)
import Text.Read (readMaybe)
import Text.Printf(printf)
import Debug.Trace

-- AM: Decided to wait with this function to try to refactor "val" function first to return 
-- a double instead of a probability distribution


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

test_policy_seq :: PolicySeq
test_policy_seq = [(DHU,Delay),(DHC,Delay),(DLU,Delay),(DLC,Delay)]

-- for input: val 0 (bi 0 1) DHU
test_expected = 
    Prob {unProb = 
                [
                    (1.0,0.44099999999999995),
                    (0.0,0.18900000000000003),
                    (0.0,0.189),
                    (0.0,8.100000000000002e-2),
                    (1.0,2.7e-2),
                    (0.0,2.999999999999999e-3),
                    (0.0,6.299999999999999e-2),
                    (0.0,6.999999999999997e-3)
                ]
            }

testVal :: Bool
testVal = undefined