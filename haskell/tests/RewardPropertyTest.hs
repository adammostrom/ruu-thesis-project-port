import Test.QuickCheck
import Data.Maybe (isJust, mapMaybe)
import Text.Read (readMaybe)
import Text.Printf(printf)
import Debug.Trace
import CoreComputation (State(..), best, Action(..), reward )



data State a = State a deriving (Show, Eq)

data Action a = Action a deriving (Show, Eq)

{-
Reward function basically returns a double. This is one property of it.
-}

