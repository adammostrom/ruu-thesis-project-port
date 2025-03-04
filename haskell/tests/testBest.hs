{-# LANGUAGE ScopedTypeVariables #-}

import Test.QuickCheck
import Data.Maybe (isJust)

-- Assuming `best` is defined in a module called `BestModule`
import CoreComputation (State(..), best, Action(..) )

-- Arbitrary instance for Action
instance Arbitrary Action where
    arbitrary = elements [Start, Delay, Unit]  -- Randomly pick one of these constructors

-- Arbitrary instance for State
instance Arbitrary State where
    arbitrary = elements [DHU, DHC, DLU, DLC, SHU, SHC, SLU, SLC]  -- Randomly pick one of these constructors

    shrink _ = []




-- First Unit Test

prop_best_0_1_DHU :: Int -> Int -> State -> Bool
prop_best_0_1_DHU x y state =
    let result = best 0 1 DHU
    in fst result == Delay && snd result == 0.4679999999997

{- -- Property: best should return a value between 0 and 1
prop_bestInRange :: Int -> Int -> ValTest.State -> Bool
prop_bestInRange x y state =
    let result = best x y state
    in result >= 0 && result <= 1

-- Property: best should be deterministic (same input -> same output)
prop_bestDeterministic :: Int -> Int -> ValTest.State -> Bool
prop_bestDeterministic x y state =
    best x y state == best x y state
-}

-- Run tests
main :: IO ()
main = do
    quickCheck prop_best_0_1_DHU

