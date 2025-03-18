module Responsibility where

import Theory
import Specification

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



-- BestExt  
bestExt :: Int -> PolicySeq a -> Policy a
bestExt t ps_tail = Map.fromList $ map bestAction states
  where
    states = [DHU, DHC, DLU, DLC]
    actions = [Start, Delay]

    -- Helper function to determine the best action for a given state
    bestAction state =
      if state `elem` states
        then
          let 
              actionValues = [(action,  (val t (Map.singleton state action : ps_tail) state)) | action <- actions]
              
              best = maximumBy (comparing snd) actionValues
           in (state, fst best)
        else (state, Start) 