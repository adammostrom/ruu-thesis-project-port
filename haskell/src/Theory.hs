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
{-|
Module      : Theory
Description : A module defining a theoretical framework for decision-making processes, including policies, states, actions, and probability distributions.
Stability   : experimental
Portability : POSIX

This module provides a typeclass `Theory` for defining decision-making models, along with utility functions for working with policies, states, actions, and probability distributions. It also includes an implementation of backwards induction for solving decision problems.

Types:
  - `Val`: Represents a numerical value, typically used for rewards or evaluations.
  - `Policy x y`: A mapping from states (`x`) to actions (`y`).
  - `PolicySeq x y`: A sequence of policies over time.
  - `Prob a`: A probability distribution monad for representing probabilistic outcomes.

Typeclass:
  - `Theory x y`: A typeclass defining the core components of a decision-making model:
      - `reward`: Computes the reward for transitioning between states given an action.
      - `next`: Defines the probability distribution over next states given a current state and action.
      - `actions`: Lists all possible actions for a given state and time step.
      - `states`: Lists all possible states in the model.

Functions:
  - `runProb`: Normalizes and collects probabilities for a given `Prob` distribution.
  - `mkSimpleProb`: Constructs a `Prob` distribution, ensuring non-negative probabilities.
  - `bi`: Implements backwards induction to compute a sequence of optimal policies.
  - `val`: Computes the value of a state under a given policy sequence.
  - `best`: Determines the best action and its value for a given state and horizon.
  - `mMeas`: Computes a measure of the difference between the best and worst policies for a state.
  - `bestExt`: Constructs the best policy for a given time step and policy sequence.
  - `worstExt`: Constructs the worst policy for a given time step and policy sequence.
  - `headPolicy`: Retrieves the first policy from a policy sequence, if available.
  - `tailPolicy`: Retrieves the tail of a policy sequence, if available.

Utilities:
  - `collectAndSumEqual`: Helper function to combine probabilities for identical outcomes.
  - `getBestAction`: Helper function to determine the best action for a state.
  - `getWorstAction`: Helper function to determine the worst action for a state.

This module is designed for use in decision-making and reinforcement learning scenarios, where policies and value functions are computed iteratively over a finite horizon.
-}

type Val = Double

type Probability = Double

type Policy x y = Map x y

type PolicySeq x y = [Policy x y]


-- Theory Typeclass
class Theory x y where

  reward :: Int -> x -> y -> x -> Val

  next :: Int -> x -> y -> Prob x

  actions :: Int -> x -> [y]

  states :: [x]



-- Probability distribution monad, (State, Probability)
newtype Prob a = Prob {unProb :: [(a, Probability)]}
  deriving (Show, Functor)

-- helper function to extract all the probabilities from the uncertainty monad
weights :: Prob a -> [Probability]
weights (Prob xs) = map snd xs

-- No negative probabilities.
validProb :: Prob a -> Bool
validProb (Prob xs) =
  let ws = weights (Prob xs) in all (>0) ws && sum ws > 0 

normalize :: Prob a -> Prob a
normalize (Prob xs) = 
  let total = sum (map snd xs )
  in if total == 0 then error "Total probability equals to zero, cannot normalize"
     else Prob [(x, p / total) | (x,p) <- xs]

prob :: (Eq a) => Prob a -> a -> Probability
prob (Prob xs) x = 
  let total = sum (map snd xs)
      matching = sum [p | (x', p) <- xs, x == x']
  in if total == 0 then 0 else matching / total

-- Implementing Monad instance
instance Applicative Prob where
  pure x = Prob [(x, 1)]
  Prob fs <*> Prob xs = Prob [(f x, p * q) | (f, p) <- fs, (x, q) <- xs]

instance Monad Prob where
  return = pure
  Prob xs >>= f = normalize . Prob $ [(y, p * q) | (x, p) <- xs, let Prob ys = normalize (f x), (y, q) <- ys]

runProb :: (Eq a) => Prob a -> [(a, Probability)]
runProb = collectAndSumEqual . unProb

collectAndSumEqual :: (Eq a) => [(a, Probability)] -> [(a, Probability)]
collectAndSumEqual aps =
  let support = nub (map fst aps)
   in map
        (\a -> (a, sum (map snd (filter ((a ==) . fst) aps))))
        support

-- Helper: Ensure probabilities are non-negative
mkSimpleProb :: [(x, Probability)] -> Prob x
mkSimpleProb = Prob . filter (\(_, p) -> p >= 0)

-- Backwards induction
bi :: Int -> Int -> PolicySeq State Action
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


bestExt ::  Int -> PolicySeq State Action -> Policy State Action
bestExt  t ps_tail = Map.fromList $ map bestAction getStates
  where
    bestAction state = (state, getBestAction t state ps_tail)
    getBestAction t state ps_tail =
      let actionsForState = actions t state
          actionValues = [(action, val t (Map.singleton state action : ps_tail) state) | action <- actionsForState]
          best = maximumBy (comparing snd) actionValues
        in fst best

worstExt ::  Int -> PolicySeq State Action -> Policy State Action
worstExt  t ps_tail = Map.fromList $ map worstAction getStates
  where
    worstAction state = (state, getWorstAction t state ps_tail)
    getWorstAction t state ps_tail =
      let actionsForState = actions t state
          actionValues = [(action, val t (Map.singleton state action : ps_tail) state) | action <- actionsForState]
          worst = minimumBy (comparing snd) actionValues
      in fst worst

headPolicy :: PolicySeq State Action-> Maybe (Policy State Action)
headPolicy [] = Nothing
headPolicy (p:ps) = Just p

tailPolicy :: PolicySeq State Action-> Maybe (PolicySeq State Action)
tailPolicy [] = Nothing
tailPolicy (p:ps) = Just ps



----------- Concrete Implementation ---------------------------------------------

data Action = Start | Delay | Unit
  deriving (Show, Eq, Enum, Ord, Read)

data State = DHU | DHC | DLU | DLC | SHU | SHC | SLU | SLC
  deriving (Show, Eq, Enum, Ord, Read)

-- Had to solve the bestExt issue with a getter.
getStates :: [State]
getStates = [DHU, DHC, DLU, DLC, SHU, SHC, SLU, SLC]

instance Theory State Action where
  states :: [State]
  states = [DHU, DHC, DLU, DLC, SHU, SHC, SLU, SLC]

  actions :: Int -> State -> [Action]
  actions _ x
    | x `elem` [DHU, DHC, DLU, DLC] = [Start, Delay]
    | x `elem` [SHU, SHC, SLU, SLC] = [Unit]
    | otherwise = error "no legitimate states"

  reward :: Int -> State -> Action -> State -> Val
  reward _ _ _ next_x = if next_x == DHU || next_x == SHU then 1 else 0


  next :: Int -> State -> Action -> Prob State
  next t x y = case (t, x, y) of
    (0, DHU, Start) ->
      mkSimpleProb
        [ (DHU, pD_Start * pH_D_DH * pU_D_0),
          (DHC, pD_Start * pH_D_DH * pC_D_0),
          (DLU, pD_Start * pL_D_DH * pU_D_0),
          (DLC, pD_Start * pL_D_DH * pC_D_0),
          (SHU, pS_Start * pH_S_DH * pU_S_0),
          (SHC, pS_Start * pH_S_DH * pC_S_0),
          (SLU, pS_Start * pL_S_DH * pU_S_0),
          (SLC, pS_Start * pL_S_DH * pC_S_0)
        ]
    (0, DHU, Delay) ->
      mkSimpleProb
        [ (DHU, pD_Delay * pH_D_DH * pU_D_0),
          (DHC, pD_Delay * pH_D_DH * pC_D_0),
          (DLU, pD_Delay * pL_D_DH * pU_D_0),
          (DLC, pD_Delay * pL_D_DH * pC_D_0),
          (SHU, pS_Delay * pH_S_DH * pU_S_0),
          (SHC, pS_Delay * pH_S_DH * pC_S_0),
          (SLU, pS_Delay * pL_S_DH * pU_S_0),
          (SLC, pS_Delay * pL_S_DH * pC_S_0)
        ]
    (0, DHC, Start) ->
      mkSimpleProb
        [ (DHC, pD_Start * pH_D_DH),
          (DLC, pD_Start * pL_D_DH),
          (SHC, pS_Start * pH_S_DH),
          (SLC, pS_Start * pL_S_DH)
        ]
    (0, DHC, Delay) ->
      mkSimpleProb
        [ (DHC, pD_Delay * pH_D_DH),
          (DLC, pD_Delay * pL_D_DH),
          (SHC, pS_Delay * pH_S_DH),
          (SLC, pS_Delay * pL_S_DH)
        ]
    (0, DLU, Start) ->
      mkSimpleProb
        [ (DHU, pD_Start * pH_D_DL * pU_D_0),
          (DHC, pD_Start * pH_D_DL * pC_D_0),
          (DLU, pD_Start * pL_D_DL * pU_D_0),
          (DLC, pD_Start * pL_D_DL * pC_D_0),
          (SHU, pS_Start * pH_S_DL * pU_S_0),
          (SHC, pS_Start * pH_S_DL * pC_S_0),
          (SLU, pS_Start * pL_S_DL * pU_S_0),
          (SLC, pS_Start * pL_S_DL * pC_S_0)
        ]
    (0, DLU, Delay) ->
      mkSimpleProb
        [ (DHU, pD_Delay * pH_D_DL * pU_D_0),
          (DHC, pD_Delay * pH_D_DL * pC_D_0),
          (DLU, pD_Delay * pL_D_DL * pU_D_0),
          (DLC, pD_Delay * pL_D_DL * pC_D_0),
          (SHU, pS_Delay * pH_S_DL * pU_S_0),
          (SHC, pS_Delay * pH_S_DL * pC_S_0),
          (SLU, pS_Delay * pL_S_DL * pU_S_0),
          (SLC, pS_Delay * pL_S_DL * pC_S_0)
        ]
    (0, DLC, Start) ->
      mkSimpleProb
        [ (DHC, pD_Start * pH_D_DL),
          (DLC, pD_Start * pL_D_DL),
          (SHC, pS_Start * pH_S_DL),
          (SLC, pS_Start * pL_S_DL)
        ]
    (0, DLC, Delay) ->
      mkSimpleProb
        [ (DHC, pD_Delay * pH_D_DL),
          (DLC, pD_Delay * pL_D_DL),
          (SHC, pS_Delay * pH_S_DL),
          (SLC, pS_Delay * pL_S_DL)
        ]
    (0, SHU, _) ->
      mkSimpleProb
        [ (SHU, pH_S_SH * pU_S_0),
          (SHC, pH_S_SH * pC_S_0),
          (SLU, pL_S_SH * pU_S_0),
          (SLC, pL_S_SH * pC_S_0)
        ]
    (0, SHC, _) ->
      mkSimpleProb
        [ (SHC, pH_S_SH),
          (SLC, pL_S_SH)
        ]
    (0, SLU, _) ->
      mkSimpleProb
        [ (SHU, pH_S_SL * pU_S_0),
          (SHC, pH_S_SL * pC_S_0),
          (SLU, pL_S_SL * pU_S_0),
          (SLC, pL_S_SL * pC_S_0)
        ]
    (0, SLC, _) ->
      mkSimpleProb
        [ (SHC, pH_S_SL),
          (SLC, pL_S_SL)
        ]
    (t, DHU, Start) ->
      mkSimpleProb
        [ (DHU, pD_Start * pH_D_DH * pU_D),
          (DHC, pD_Start * pH_D_DH * pC_D),
          (DLU, pD_Start * pL_D_DH * pU_D),
          (DLC, pD_Start * pL_D_DH * pC_D),
          (SHU, pS_Start * pH_S_DH * pU_S),
          (SHC, pS_Start * pH_S_DH * pC_S),
          (SLU, pS_Start * pL_S_DH * pU_S),
          (SLC, pS_Start * pL_S_DH * pC_S)
        ]
    (t, DHU, Delay) ->
      mkSimpleProb
        [ (DHU, pD_Delay * pH_D_DH * pU_D),
          (DHC, pD_Delay * pH_D_DH * pC_D),
          (DLU, pD_Delay * pL_D_DH * pU_D),
          (DLC, pD_Delay * pL_D_DH * pC_D),
          (SHU, pS_Delay * pH_S_DH * pU_S),
          (SHC, pS_Delay * pH_S_DH * pC_S),
          (SLU, pS_Delay * pL_S_DH * pU_S),
          (SLC, pS_Delay * pL_S_DH * pC_S)
        ]
    (t, DHC, Start) ->
      mkSimpleProb
        [ (DHC, pD_Start * pH_D_DH),
          (DLC, pD_Start * pL_D_DH),
          (SHC, pS_Start * pH_S_DH),
          (SLC, pS_Start * pL_S_DH)
        ]
    (t, DHC, Delay) ->
      mkSimpleProb
        [ (DHC, pD_Delay * pH_D_DH),
          (DLC, pD_Delay * pL_D_DH),
          (SHC, pS_Delay * pH_S_DH),
          (SLC, pS_Delay * pL_S_DH)
        ]
    (t, DLU, Start) ->
      mkSimpleProb
        [ (DHU, pD_Start * pH_D_DL * pU_D),
          (DHC, pD_Start * pH_D_DL * pC_D),
          (DLU, pD_Start * pL_D_DL * pU_D),
          (DLC, pD_Start * pL_D_DL * pC_D),
          (SHU, pS_Start * pH_S_DL * pU_S),
          (SHC, pS_Start * pH_S_DL * pC_S),
          (SLU, pS_Start * pL_S_DL * pU_S),
          (SLC, pS_Start * pL_S_DL * pC_S)
        ]
    (t, DLU, Delay) ->
      mkSimpleProb
        [ (DHU, pD_Delay * pH_D_DL * pU_D),
          (DHC, pD_Delay * pH_D_DL * pC_D),
          (DLU, pD_Delay * pL_D_DL * pU_D),
          (DLC, pD_Delay * pL_D_DL * pC_D),
          (SHU, pS_Delay * pH_S_DL * pU_S),
          (SHC, pS_Delay * pH_S_DL * pC_S),
          (SLU, pS_Delay * pL_S_DL * pU_S),
          (SLC, pS_Delay * pL_S_DL * pC_S)
        ]
    (t, DLC, Start) ->
      mkSimpleProb
        [ (DHC, pD_Start * pH_D_DL),
          (DLC, pD_Start * pL_D_DL),
          (SHC, pS_Start * pH_S_DL),
          (SLC, pS_Start * pL_S_DL)
        ]
    (t, DLC, Delay) ->
      mkSimpleProb
        [ (DHC, pD_Delay * pH_D_DL),
          (DLC, pD_Delay * pL_D_DL),
          (SHC, pS_Delay * pH_S_DL),
          (SLC, pS_Delay * pL_S_DL)
        ]
    (t, SHU, Unit) ->
      mkSimpleProb
        [ (SHU, pH_S_SH * pU_S),
          (SHC, pH_S_SH * pC_S),
          (SLU, pL_S_SH * pU_S),
          (SLC, pL_S_SH * pC_S)
        ]
    (t, SHC, Unit) ->
      mkSimpleProb
        [ (SHC, pH_S_SH),
          (SLC, pL_S_SH)
        ]
    (t, SLU, Unit) ->
      mkSimpleProb
        [ (SHU, pH_S_SL * pU_S),
          (SHC, pH_S_SL * pC_S),
          (SLU, pL_S_SL * pU_S),
          (SLC, pL_S_SL * pC_S)
        ]
    (t, SLC, Unit) ->
      mkSimpleProb
        [ (SHC, pH_S_SL),
          (SLC, pL_S_SL)
        ]
    _ -> error "Invalid state or action combination"



pS_Start :: Double
pS_Start = 0.9

pD_Start :: Double
pD_Start = (1.0 - pS_Start)

pD_Delay :: Double
pD_Delay = 0.9

pS_Delay :: Double
pS_Delay = (1.0 - pD_Delay)

pL_S_DH :: Double
pL_S_DH = 0.7

pL_S_DL :: Double
pL_S_DL = 0.9

pL_S_SL :: Double
pL_S_SL = 0.7

pL_S_SH :: Double
pL_S_SH = 0.3

pL_D_DH :: Double
pL_D_DH = pL_S_SH

pL_D_DL :: Double
pL_D_DL = (pL_S_SL)

pU_S_0 :: Double
pU_S_0 = 0.9

pU_D_0 :: Double
pU_D_0 = 0.7

pU_S :: Double
pU_S = 0.9

pU_D :: Double
pU_D = 0.3

pH_S_DH :: Double
pH_S_DH = (1.0 - pL_S_DH)

pH_S_SH :: Double
pH_S_SH = (1.0 - pL_S_SH)

pH_S_DL :: Double
pH_S_DL = (1.0 - pL_S_DL)

pH_S_SL :: Double
pH_S_SL = (1.0 - pL_S_SL)

pH_D_DH :: Double
pH_D_DH = (1.0 - pL_D_DH)

pH_D_DL :: Double
pH_D_DL = (1.0 - pL_D_DL)

pC_S_0 :: Double
pC_S_0 = (1.0 - pU_S_0)

pC_D_0 :: Double
pC_D_0 = (1.0 - pU_D_0)

pC_S :: Double
pC_S = (1.0 - pU_S)

pC_D :: Double
pC_D = (1.0 - pU_D)





  
  




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