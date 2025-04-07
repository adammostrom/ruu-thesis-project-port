{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies, KindSignatures #-}

module Theory where

import Data.List (maximumBy, minimumBy, nub)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

type Val = Double

type Policy x y = Map x y

type PolicySeq x y = [Policy x y]

class Theory x y where

  reward :: Int -> x -> y -> x -> Val

  next :: Int -> x -> y -> Prob x

  actions :: Int -> x -> [y]

  states :: [x]

  --bestExt :: Int -> PolicySeq x y -> Policy x y

  worstExt :: Int -> PolicySeq x y -> Policy x y



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

runProb :: (Eq a) => Prob a -> [(a, Double)]
runProb = collectAndSumEqual . unProb

collectAndSumEqual :: (Eq a) => [(a, Double)] -> [(a, Double)]
collectAndSumEqual aps =
  let support = nub (map fst aps)
   in map
        (\a -> (a, sum (map snd (filter ((a ==) . fst) aps))))
        support

-- Helper: Ensure probabilities are non-negative
mkSimpleProb :: [(x, Double)] -> Prob x
mkSimpleProb = Prob . filter (\(_, p) -> p >= 0)

-- Backwards induction
bi :: Int -> Int -> PolicySeq x y
bi _ 0 = []
bi t n =
  let ps_tail = bi t (n - 1)
      p = bestExt t ps_tail
   in p : ps_tail

val :: (Theory x y, Ord x, Show x) => Int -> PolicySeq x y -> x -> Val
val _ [] _ = 0
val t (p : ps) x =
  case Map.lookup x p of
    Nothing -> error $ "No action found in policy for state: " ++ show x
    Just y ->
      let mNext = runProb $ next t x y
      in sum [pr * (reward t x y x' + val (t + 1) ps x') | (x', pr) <- mNext]


best :: Int -> Int -> State -> (Action, Val)
best t n x
    | n == 0 = error "Horizon must be greater than zero!"
    | otherwise =
        let ps = bi (t + 1) (n - 1)
            p = bestExt t ps
            b = fromMaybe (error "No action found!") (Map.lookup x p)
            vb = val t (p : ps) x
        in (b, vb)


mMeas :: Int -> Int -> State -> Double
mMeas t n x
  | t < 0 || n < 0        = error "t and n must be non-negative"
  | otherwise =
    let psTail = bi (t + 1) (n - 1)
        pBest  = bestExt t psTail
        pWorst = worstExt t psTail
        bestVal  = val t (pBest : psTail) x
        worstVal = val t (pWorst : psTail) x
    in if bestVal == 0 then 0 else (bestVal - worstVal) / bestVal



----------- Concrete Implementation ---------------------------------------------

data Action = Start | Delay | Unit
  deriving (Show, Eq, Enum, Ord, Read)

data State = DHU | DHC | DLU | DLC | SHU | SHC | SLU | SLC
  deriving (Show, Eq, Enum, Ord, Read)

-- Had to solve the bestExt issue with a getter.
getStates :: [State]
getStates = [DHU, DHC]

instance Theory State Action where
  states :: [State]
  states = [DHU, DHC]

  actions :: Int -> State -> [Action]
  actions _ x
    | x `elem` [DHU] = [Start, Delay]
    | x `elem` [DHC] = [Delay, Start]
    | otherwise = error "no legitimate states"

  reward :: Int -> State -> Action -> State -> Val
  reward _ _ _ next_x = if next_x == DHU then 1 else 0

  next :: Int -> State -> Action -> Prob State
  next t x y = case (t, x, y) of
    (0, DHC, Start) -> mkSimpleProb [(DHU, 0.1)]
    (0, DHC, Delay) -> mkSimpleProb [(DHU, 0.9)]
    (1, DHC, Start) -> mkSimpleProb [(DHC, 0.8)]
    (1, DHC, Delay) -> mkSimpleProb [(DHC, 0.8)]
    (0, DHU, Start) -> mkSimpleProb [(DHU, 0.1)]
    (0, DHU, Delay) -> mkSimpleProb [(DHU, 0.9)]
    (1, DHU, Start) -> mkSimpleProb [(DHC, 0.8)]
    (1, DHU, Delay) -> mkSimpleProb [(DHC, 0.8)]




  worstExt ::  Int -> PolicySeq State Action -> Policy State Action
  worstExt  t ps_tail = Map.fromList $ map worstAction getStates
    where
      worstAction state = (state, getWorstAction t state ps_tail)
      getWorstAction t state ps_tail =
        let actionsForState = actions t state
            actionValues = [(action, val t (Map.singleton state action : ps_tail) state) | action <- actionsForState]
            worst = minimumBy (comparing snd) actionValues
        in fst worst
  
  
  bestExt ::  Int -> PolicySeq State Action -> Policy State Action
  bestExt  t ps_tail = Map.fromList $ map bestAction getStates
    where
      bestAction state = (state, getBestAction t state ps_tail)
      getBestAction t state ps_tail =
        let actionsForState = actions t state
            actionValues = [(action, val t (Map.singleton state action : ps_tail) state) | action <- actionsForState]
            best = maximumBy (comparing snd) actionValues
          in fst best



------- \\\ TESTING \\\ --------------------------------------------------------
pol :: Policy State Action
pol = Map.fromList [(DHU, Delay)]

pol2 :: Policy State Action
pol2 = Map.fromList [(DHC, Start)]

pol3 :: Policy State Action
pol3 = Map.fromList [(DHU, Delay)]

polseq :: PolicySeq State Action
polseq = [pol, pol2, pol3]

simplePol :: PolicySeq State Action
simplePol = [Map.fromList [(DHU, Start), (DHC, Delay)]]

biTest :: PolicySeq State Action
biTest = bi 0 1