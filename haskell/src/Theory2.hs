{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Theory where

import Data.List (nub, maximumBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)




type Val = Double
-- A policy maps states to actions
type Policy x y = Map x y
-- A sequence of policies
type PolicySeq x y = [Policy x y]

-- Probability distribution monad
newtype Prob a = Prob {unProb :: [(a, Double)]}
  deriving (Show, Functor)


-- Implementing Monad instance
instance Applicative Prob where
  pure x = Prob [(x, 1)] 
  Prob fs <*> Prob xs = Prob [(f x, p * q) | (f, p) <- fs, (x, q) <- xs]

instance Monad Prob where
  return = pure
  Prob xs >>= f = Prob [(y, p * q) | (x, p) <- xs, (y, q) <- unProb (f x)]


runProb :: Eq a => Prob a -> [(a, Double)]
runProb = collectAndSumEqual . unProb

collectAndSumEqual :: Eq a => [(a,Double)] -> [(a, Double)]
collectAndSumEqual aps =
  let support = nub (map fst aps)
  in map (\a -> (a, sum (map snd (filter ((a==).fst) aps))))
          support

class Theory x y where


    reward :: Int -> x -> y -> x -> Val
    next :: Int -> x -> y -> Prob x
    -- Available actions for a given state at time t
    actions :: Int -> x -> [y]  
    -- List of states
    states :: [x]

    
val :: (Theory x y, Ord x, Show x) => Int -> PolicySeq x y -> x -> Val
val _ [] _ = 0
val t (p : ps) x =
    case Map.lookup x p of
        Nothing -> error $ "No action found in policy for state: " ++ show x
        Just y ->
            let mNext = runProb $ next t x y
            in sum [pr * (reward t x y x' + val (t + 1) ps x') | (x', pr) <- mNext]


{- -- Backwards induction
bi :: Int -> Int -> PolicySeq x y
bi _ 0 = []
bi t n =
    let ps_tail = bi t (n - 1)
        p = bestExt t ps_tail
    in p : ps_tail
 -}


bestExt :: (Theory x y, Ord x, Show x) => Int -> PolicySeq x y -> Policy x y
bestExt t ps_tail = Map.fromList $ map bestAction states   -- states comes from the Theory context
  where
    bestAction state = (state, getBestAction t state ps_tail)

-- Helper function to get the best action for a given state
getBestAction :: (Theory x y, Ord x, Show x) => Int -> x -> PolicySeq x y -> y
getBestAction t state ps_tail =
    let actionsForState = actions t state
        actionValues = [(action, val t (Map.singleton state action : ps_tail) state) | action <- actionsForState]
        best = maximumBy (comparing snd) actionValues
    in fst best

    



 -- Helper: Ensure probabilities are non-negative
mkSimpleProb :: [(x, Double)] -> Prob x
mkSimpleProb = Prob . filter (\(_, p) -> p >= 0)

---------------------------------------------------------------------------------


data Action = Start | Delay deriving (Eq, Show)

data State = DHU | DHC deriving (Eq, Show, Ord)
    
instance Theory State Action where



    states :: [State]
    states = [DHU, DHC]

    actions :: Int -> State -> [Action]
    actions t x = case (t,  x) of
        (0, DHU) -> [Start, Delay]
        (0, DHC) -> [Delay, Delay]

    reward :: Int -> State -> Action -> State -> Val
    reward _ _ _ next_x = if next_x == DHU then 1 else 0

    next :: Int -> State -> Action -> Prob State
    next t x y =  case (t, x, y) of
        (0, DHC, Start) -> mkSimpleProb [(DHU, 0.1)]
        (1, DHC, Delay) -> mkSimpleProb [(DHU, 0.9)]
        (0, DHU, Delay) -> mkSimpleProb [(DHC, 0.8)]
        (1, DHU, Delay) -> mkSimpleProb [(DHC, 0.8)]




------- \\\ TESTING \\\ --------------------------------------------------------
pol :: Policy State Action
pol = Map.fromList [(DHU, Delay)]

pol2 :: Policy State Action
pol2 = Map.fromList [(DHC, Start)]

pol3 :: Policy State Action
pol3 = Map.fromList [(DHU, Delay)]


polseq :: PolicySeq State Action
polseq = [ pol, pol2, pol3]


simplePol :: PolicySeq State Action
simplePol = [Map.fromList [(DHU, Start), (DHC, Delay)]]
