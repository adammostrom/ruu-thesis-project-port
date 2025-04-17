module Theory where

-- import Rel.TotalPreorder

-- NOTE: Idris code marked in comments as beginning with ">".

import Control.Monad (liftM2)
import Probabilities

--------------------------------------------------------------------
-- Theory.hs
type Time = Int     -- Time discreet MDP:s motivate an integer time type.

type Log = String   -- The scripts will log to the terminal with strings

type Val = Double -- Val is the type of rewards

data State = Nil | State deriving (Show, Eq, Ord) -- States must be valid, otherwise fall onto Nil

data Control t x = Control t x deriving (Show)  -- A control is a time and state dependent name of a policy

type Policy x x' = Map x x'     -- A policy is a mapping between states

availableControls ::  Time -> State -> [Control]

states :: Control -> [State]

-- Meas is a specification function defined by the case. Later we will set meas to compute expected value
meas :: [(Val, Probability)] -> Val
-- meas rpPairs = sum [xi * pi | (xi, pi) <- rpPairs]

reward :: Time -> State -> Control -> State -> Val
-- reward t x y x' = if (isCommitted x' || isDisrupted x') then 0 else 1


-- The value of a policy sequence is defined inductively as the measuee of an M structure of Val values
val :: Time -> State -> Int -> [Control] -> Val
val t x n ys = foldl vals 0 ys
  where
    vals :: Val -> Control -> Val
    vals acc y = acc + meas [(reward t x y s', p) | (s', p) <- next t x y]
        -- vals = meas $ zipWith (reward t x) ys (fst x')
val t x n (y:ys) = thisReward + val (succ t) x' (pred n) ys
    where
        x' = next t x y -- Problem it is not a state but a probability monad, Adam solves w runProb
        thisReward = meas zipWith (reward t x) ys (fst x') -- The measure of mapping partially applied reward onto set of next states with their corresponding probabilities.
        -- Note that map (reward t x y) 


bi :: Time -> Int -> [Control]
bi _ 0 = []
bi t n = p:ps   -- Yeilds yn : yn-1 : .. : y1 : [] ; s.t. all are optimal controls
    where
        ps = bi (succ t) (pred n)  -- bi(t, n-1) |-> yn-1 : .. : y1 : []
        p = chooseBestExtension t ps    -- Tag onto it; chooseBestExtension(t, \vec{y}) |-> yn

-- chooseBestExtension ps is an optimal policy, a fnc from states to controls, at decision step t.
chooseBestExtension :: Time -> Int -> [Control] -> Policy
chooseBestExtension t n ys = Map.fromList [(x, bestCtrl x) | x <- concatMap states ys] -- This comprehension flattens the results of states $ y for all y in ys into a single list of (x, bestCtrl x) pairs.
    where
        bestCtrl :: State -> Control
        bestCtrl x = fst $ maximumBy (comparing snd) [(y, val t x n y) | y <- availableControls x]

{- 
∗ Entry point
∗ The task is to answer a query of which are the optimal set of controls to apply at times t for an n-horizon given that we start in state x.
∗ If this horizon is zero we just handle that by pattern matching.
∗ If it is best(t, n, x) we...
    ∗ Build a network of state nodes in (t + 1)
-}
best :: Time -> Int -> State -> Log
best _ 0 _ = "The horizon must be greater than zero!"
best t n x = "Horizon, best, value : " ++ show (succ n) ++ ", " ++ show b ++ ", " ++ show vb
  where
    ys = bi t n     -- Adam takes n-1 twice??; Here I extract a list of plausible control sequences
    p = chooseBestExtension t n ys  -- Feeding controls into this finds the best n-step extension sequence.
    b = p x
    vb = val (p:ps)