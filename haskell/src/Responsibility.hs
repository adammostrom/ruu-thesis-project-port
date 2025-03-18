module Responsibility where

import Theory

-- best 
-- Compute the best action and expected value for a given time, state, and horizon
best :: Int -> Int -> State -> (Action, Val)
best t n x
  | n == 0 = error "Horizon must be greater than zero!"
  | otherwise =
      let ps = bi (t + 1) (n - 1)
          p = best_ext t ps
          b = fromMaybe Unit (Map.lookup x p)
          vb =  (val t (p : ps) x)
       in (b, vb)


-- bests

-- mMeas
mMeas :: Int -> Int -> State -> Double
mMeas t n x
  | x `elem` [SHU, SHC, SLU, SLC] = 0
  | otherwise =
      let ps = bi t n
          ps' =
            if Map.lookup x (head ps) == Just Start
              then (Map.insert x Delay (head ps)) : tail ps
              else (Map.insert x Start (head ps)) : tail ps
          bestVal =  (val t ps x)
          worstVal =  (val t ps' x)
       in (bestVal - worstVal) / bestVal