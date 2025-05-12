{-|
Module      : SDPCompute
Description : Core Stochastic Dynamic Programming (SDP) algorithms
Copyright   : (c) Group 12, 2025
License     : MIT

This module implements the core algorithms for solving Stochastic Dynamic Programming problems.
It provides both standard and memoized versions of key SDP operations:

Core Functions:
* Value Function Computation (val, val')
  - Evaluates policy sequences
  - Supports arbitrary state and action types
  - Memoized version for performance

* Backward Induction (bi, bi')
  - Generates optimal policy sequences
  - Supports variable horizon lengths
  - Memoized version for large state spaces

* Policy Extensions (bestExt, worstExt)
  - Computes optimal/pessimal policy extensions
  - Supports custom ordering functions
  - Memoized versions available

* State Measurements (mMeas, mMeas')
  - Computes state importance metrics
  - Compares best/worst achievable values
  - Supports decision analysis

Implementation Details:
- Uses Map for efficient state-action mappings
- Supports generic state and action types
- Provides both recursive and memoized implementations
- Thread-safe pure functional implementation
-}
module SDPCompute (module SDPCompute, module SDPTypes) where
import Data.List (maximumBy, minimumBy, nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Function (on)
import Data.Ord (comparing)
import Prob(Prob, runProb, expectation)
import SDPTypes

{-| Compute the value of a policy sequence from a given state and time.
    Uses recursive computation without memoization.

    @
    val sdp t [] x = 0
    val sdp t (p:ps) x = E[r(t,x,y,X') + val(t+1,ps,X')] where X'~P(·|t,x,y)
    @
-}
val :: (Ord x, Show x) => SDP x y -> Int -> PolicySeq x y -> x -> Val
val sdp _ [] _         = 0
val sdp t _ _  | t < 0 = error "Time cannot be negative" 
val sdp t (p : ps) x   =
  case Map.lookup x p of
    Nothing -> error $ "No action found in policy for state: " ++ show x
    Just y -> 
      let mNext = next sdp t x y
          recCall x' = reward sdp t x y x' + val sdp (t + 1) ps x'
      in expectation recCall mNext

----------------------------- Memoization val --------------------------------

{-| A table mapping states to their computed values at a given decision step.
    Used for memoization to avoid redundant recursive computations.
-}
type ValMap x = Map x Val

{-| Create initial value map for the terminal time step.
    All states are assigned zero value at the horizon.
-}
baseValMap :: (Ord x, Show x) => SDP x y -> Int -> ValMap x
baseValMap sdp tEnd = Map.fromList [ (s, 0) | s <- states sdp tEnd ]

{-| Perform one step of backward induction to compute state values.
    Given the value map at t+1, computes the value map at time t under policy p.
-}
stepValMap :: (Ord x, Show x) => SDP x y -> (Policy x y, Int) -> ValMap x -> ValMap x
stepValMap sdp (p, t) vNext = Map.fromList [ (s, eval s) | s <- states sdp t] where
  eval s = case Map.lookup s p of
    Nothing -> error $ "No action found for state " ++ show s
    Just y  ->  let 
                  mNext = next sdp t s y
                in expectation (\s' -> reward sdp t s y s' + vNext Map.! s') mNext

{-| Build the complete table of state values through backward induction.
    Folds backward through the policy sequence to compute values at all times.
-}
valMaps :: (Ord x, Show x) => SDP x y -> Int -> PolicySeq x y -> ValMap x
valMaps sdp t0 ps = foldr (stepValMap sdp) (baseValMap sdp tEnd) (zip ps [t0 ..])
  where
    tEnd = t0 + length ps

{-| Memoized version of the value function.
    Uses pre-computed value tables for efficiency.
-}
val' :: (Ord x, Show x) => SDP x y -> Int -> PolicySeq x y -> x -> Val
val' sdp t0 ps x = 
  let 
    table = valMaps sdp t0 ps
  in fromMaybe (error $ "val': unknown state " ++ show x) (Map.lookup x table)

----------------------------------------------------------------------------------------------------------

{-| Generate optimal policy sequence using backward induction.
    Non-memoized version.
-}
bi :: (Show x, Ord x) => SDP x y -> Int -> Int -> PolicySeq x y
bi sdp _ 0 = []
bi sdp t n =
  let  ps_tail = bi sdp t (n - 1)
       p = bestExt sdp t ps_tail
   in  p : ps_tail

{-| Generate optimal policy sequence using backward induction.
    Memoized version for better performance.
-}
bi' :: (Show x, Ord x) => SDP x y -> Int -> Int -> PolicySeq x y
bi' sdp _ 0 = []
bi' sdp t n =
  let  ps_tail = bi' sdp t (n - 1)
       p = bestExt' sdp t ps_tail
   in  p : ps_tail

{-| Compute optimal policy extension based on comparison function.
    Non-memoized version.
-}
optExt :: (Show x, Ord x) => (Val->Val->Ordering) -> SDP x y -> Int -> PolicySeq x y -> Policy x y
optExt cmp sdp t ps = Map.fromList $ map optAction (states sdp t)
  where
    optAction state = (state, getOptAction t state ps)
    getOptAction t state ps =
      let  actionsForState = actions sdp t state
           actionValues = [  (action, val sdp t (Map.singleton state action : ps) state)
                          |  action <- actionsForState
                          ]
           opt = maximumBy (cmp `on` snd) actionValues
      in fst opt

optExt' :: (Show x, Ord x) => (Val -> Val -> Ordering) -> SDP x y -> Int -> PolicySeq x y -> Policy x y
optExt' cmp sdp t ps = Map.fromList $ map optAction (states sdp t)
  where
    optAction state = (state, getOptAction t state ps)
    getOptAction t state ps =
      let  actionsForState = actions sdp t state
           actionValues = [  (action, val' sdp t (Map.singleton state action : ps) state) 
                          |  action <- actionsForState
                          ]
           opt = maximumBy (cmp `on` snd) actionValues
      in fst opt

bestExt :: (Show x, Ord x) => SDP x y -> Int -> PolicySeq x y -> Policy x y
bestExt = optExt compare

bestExt' :: (Show x, Ord x) => SDP x y -> Int -> PolicySeq x y -> Policy x y
bestExt' = optExt' compare

flipOrder :: Ordering -> Ordering
flipOrder GT = LT
flipOrder LT = GT
flipOrder EQ = EQ

worstExt :: (Show x, Ord x) => SDP x y -> Int -> PolicySeq x y -> Policy x y
worstExt = optExt (\x y -> flipOrder (compare x y))

worstExt' :: (Show x, Ord x) => SDP x y -> Int -> PolicySeq x y -> Policy x y
worstExt' = optExt' (\x y -> flipOrder (compare x y))

{-| Find best action and its value for a given state.
    Non-memoized version.
-}
best :: (Show x, Ord x) => SDP x y -> Int -> Int -> x -> (y, Val)
best sdp t n x
    | n == 0 = error "Horizon must be greater than zero!"
    | otherwise =
        let  ps  = bi sdp (t + 1) (n - 1)
             p   = bestExt sdp t ps
             b   = fromMaybe (error "No action found!") (Map.lookup x p)
             vb  = val sdp t (p : ps) x
        in (b, vb)

{-| Find best action and its value for a given state.
    Memoized version for better performance.
-}
best' :: (Show x, Ord x) => SDP x y -> Int -> Int -> x -> (y, Val)
best' sdp t n x
    | n == 0 = error "Horizon must be greater than zero!"
    | otherwise =
        let  ps  = bi' sdp (t + 1) (n - 1)
             p   = bestExt' sdp t ps
             b   = fromMaybe (error "No action found!") (Map.lookup x p)
             vb  = val' sdp t (p : ps) x
        in (b, vb)

{-| Compute importance measure for a state in decision making.
    Compares best and worst achievable values to determine state significance.
    Non-memoized version.
-}
mMeas :: (Show x, Ord x) => SDP x y -> Int -> Int -> x -> Double
mMeas sdp t n x =
      let  ps = bi sdp t n
           bestVal = val sdp t ps x
           psTail = tail ps
           worstPolicy | null ps = error "mMeas needs at least one step but got an empty PolicySeq"
                       | otherwise = worstExt sdp t psTail
           worstVal = val sdp t (worstPolicy : psTail) x
      in if bestVal == 0 && worstVal == 0 then 0
         else (bestVal - worstVal) / bestVal

{-| Compute importance measure for a state in decision making.
    Compares best and worst achievable values to determine state significance.
    Memoized version for better performance.
-}
mMeas' :: (Show x, Ord x) => SDP x y -> Int -> Int -> x -> Double
mMeas' sdp t n x =
      let  ps = bi' sdp t n
           bestVal = val' sdp t ps x
           psTail = tail ps
           worstPolicy | null ps = error "mMeas needs at least one step but got an empty PolicySeq"
                       | otherwise = worstExt' sdp t psTail
           worstVal = val' sdp t (worstPolicy : psTail) x
      in if bestVal == 0 && worstVal == 0 then 0
         else (bestVal - worstVal) / bestVal