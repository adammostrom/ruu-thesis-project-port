import Test.QuickCheck
import Data.Maybe (isJust, mapMaybe)
import Text.Read (readMaybe)
import Text.Printf(printf)

-- Assuming `best` is defined in a module called `BestModule`
import CoreComputation (State(..), best, Action(..) )



-- First Unit Test

prop_best_0_1_DHU :: Int -> Int -> State -> Bool
prop_best_0_1_DHU x y state =
    let result = best 0 1 DHU
    in fst result == Delay && snd result == 0.4679999999997


-- Run tests
main :: IO ()
main = do
    quickCheck prop_best_0_1_DHU 

-- Function to parse a line from the text file
