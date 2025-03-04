{-# LANGUAGE ScopedTypeVariables #-}

import Test.QuickCheck
import Data.Maybe (isJust)

-- Assuming `best` is defined in a module called `BestModule`
import ValTest (best)

-- Property: best should return a value between 0 and 1
prop_bestInRange :: Int -> Int -> String -> Bool
prop_bestInRange x y str =
    let result = best x y str
    in result >= 0 && result <= 1

-- Property: best should be deterministic (same input -> same output)
prop_bestDeterministic :: Int -> Int -> String -> Bool
prop_bestDeterministic x y str =
    best x y str == best x y str

-- Run tests
main :: IO ()
main = do
    quickCheck prop_bestInRange
    quickCheck prop_bestDeterministic
