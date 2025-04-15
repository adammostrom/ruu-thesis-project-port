module Test_matterMost where

import Specification
import Test.QuickCheck
import Theory hiding (Action, State)

instance Arbitrary State where
  arbitrary = elements [DHU, DHC, DLU, DLC, SHU, SHC, SLU, SLC]

instance Arbitrary Action where
  arbitrary = elements [Start, Delay, Unit]

----------------- Unit Tests ---------------------------------------------------
{-
    Idris Results:

    *Responsibility> :exec best 0 1 DHU
    "Horizon, best, value:  1,  Delay,  0.468"
-}
unit_best :: Bool
unit_best =
  let result = bestWrap 0 1 DHU
   in fst result == Delay && snd result >= 0.467 && snd result <= 0.469

----------------- Property Tests -----------------------------------------------

-- Tests that the policy we get for bestExt is "better" than that of worstExt
prop_best_geq_worst :: State -> Property
prop_best_geq_worst s =
  forAll (choose (1, 8)) $ \h ->
    let ps = biWrap 0 h
        bestPol = bestExtWrap 0 ps
        worstPol = worstExtWrap 0 ps

        bestVal = valWrap 0 (bestPol : ps) s
        worstVal = valWrap 0 (worstPol : ps) s
     in bestVal >= worstVal

-- Val returns positive values
prop_val_non_negative :: State -> Property
prop_val_non_negative s = forAll (choose (1, 8)) $ \h ->
  let ps = biWrap 0 h
      v = valWrap 0 ps s
   in v >= 0

-- best functions returns valid actions for that state
prop_best_gives_valid_action :: State -> Property
prop_best_gives_valid_action s =
  forAll (choose (1, 5)) $ \h ->
    let (a, _) = bestWrap 0 h s
        validActions = actions h s
     in a `elem` validActions

-- Test that the state provided by next is subsequent state of the previous state
prop_deterministic_next :: Int -> PolicySeq State Action -> State -> Property
prop_deterministic_next t (p:ps) x = 
    case Map.lookup x p of
        Nothing -> property True
        Just y ->
            expected = reward t x y x' + val (t + 1) ps x'
            in val t (p:ps) x === expected



main :: IO ()
main = do
  putStrLn $ "Running test: Val return not negative: "
  quickCheck prop_val_non_negative
  putStrLn $ "Running test: Best returns a valid action for given state:  "
  quickCheck prop_best_gives_valid_action
  quickCheck prop_best_geq_worst
