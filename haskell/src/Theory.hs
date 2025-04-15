module Theory where

-- import Rel.TotalPreorder

-- NOTE: Idris code marked in comments as beginning with ">".

import Control.Monad (liftM2)
import Probabilities

--------------------------------------------------------------------
-- Theory.hs
type Time = Int

type Policy = Map

type Log = String

type Val = Double -- Val is the type of rewards

data State = Nil | State deriving (Show, Eq, Ord)

data Control t x = Control State x deriving (Show)

type PolicySeq = [(Control, Val)]

-- Available controls: given a time & state, return a list of valid controls
availableControls ::  Time -> State -> [Control]

-- Reward function
reward :: X a => a -> Control a b -> a -> Val

-- Meas is a specification function defined by the case. Later we will set meas to compute expected value of 
meas :: [(a, Val)] -> Val -- Vettefan


reward :: Time -> State -> Control -> State -> Val

-- The value of a policy sequence is defined inductively as the measuee of an M structure of Val values
val :: [Control] -> Val
val [] = 0
val (p:ps) = meas p + val ps



bi :: Time -> Int -> [Control]
bi _ 0 = []
bi t n = p:ps 
    where
        ps = bi t (pred n)
        p = bestExt t ps

-- bestExt ps is an optimal policy, a fnc from states to controls, at decision step t.
-- Defining bestExt in a partial-application-way makes sense
bestExt :: Time -> Int -> state -> Control State   -- e.g. Start DHU

--bestExt :: Time -> Int -> [Control] -> Control State   -- e.g. Start DHU
--bestExt t n pol@(p:ps) = max $ (snd [(y, r) <- pol, r = reward t x y x', y]
--    where
--        y = -- Control such that val is maximized
--        x = 

best :: Time -> Int -> State -> Log
best _ 0 _ = "The horizon must be greater than zero!"
best t n x = "Horizon, best, value : " ++ show (succ n) ++ ", " ++ show b ++ ", " ++ show vb
  where
    ps = bi (succ t) (succ n)
    p = bestExt ps
    b = p x
    vb = val (p:ps)