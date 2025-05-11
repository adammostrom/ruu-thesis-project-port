module SDPCompute (module SDPCompute, module SDPTypes) where
import Data.List (maximumBy, minimumBy, nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Function (on)
import Data.Ord (comparing)
import Prob(Prob, runProb, expectation)
import SDPTypes

val :: (Ord x, Show x) => SDP x y -> Int -> PolicySeq x y -> x -> Val
val sdp _ [] _ = 0
val sdp t (p : ps) x =
  case Map.lookup x p of
    Nothing -> error $ "No action found in policy for state: " ++ show x
    Just y ->
      let mNext = next sdp t x y
          recCall x' = reward sdp t x y x' + val sdp (t + 1) ps x'
      in expectation recCall mNext

----------------------------- Memoization val --------------------------------
{-
Idea: pre-compute the value function for all states at each time step.
∗ It is a table of size |states| x |time|, where |time| is the length of the policy sequence.
∗ The table is built by backward induction, starting from the end of the horizon.
∗ The table is built by folding backward through the policy sequence.
  This was nesseacry to factor out the recursion in the val function.
-}

-- A table mapping each state to its computed value at a given decision step.
type ValMap x = Map x Val

-- Base value map (base case fir finite horizon): at the end of the horizon, every state has value 0.
baseValMap :: (Ord x, Show x) => SDP x y -> Int -> ValMap x
baseValMap sdp tEnd = Map.fromList [ (s, 0) | s <- states sdp tEnd ]
--              ^ t0 + length ps

-- One backward induction step: given the map at time t+1, compute the map at time t under policy p.
stepValMap :: (Ord x, Show x) => SDP x y -> (Policy x y, Int) -> ValMap x -> ValMap x
--                                             ^ policy for time t  |^ map @ t+1|^ map @ t
stepValMap sdp (p, t) vNext = Map.fromList [ (s, eval s) | s <- states sdp t] where
  eval s = case Map.lookup s p of
    Nothing -> error $ "No action found for state " ++ show s
    Just y  ->  let 
                  mNext = next sdp t s y
                in expectation (\s' -> reward sdp t s y s' + vNext Map.! s') mNext
--              Idea (optimization): use a fold instead of a list comprehension to compute the map.

-- Build the full table of state-values at the starting time t0 by folding backward through the policy sequence.
valMaps :: (Ord x, Show x) => SDP x y -> Int -> PolicySeq x y -> ValMap x
valMaps sdp t0 ps = foldr (stepValMap sdp) (baseValMap sdp tEnd) (zip ps [t0 ..])
  where
    tEnd = t0 + length ps

-- Memoized value-function: look up the pre-computed table.
val' :: (Ord x, Show x) => SDP x y -> Int -> PolicySeq x y -> x -> Val
val' sdp t0 ps x = 
  let 
    table = valMaps sdp t0 ps
  in fromMaybe (error $ "val': unknown state " ++ show x) (Map.lookup x table)

----------------------------------------------------------------------------------------------------------
-- Backwards induction
bi :: (Show x, Ord x) => SDP x y -> Int -> Int -> PolicySeq x y
bi sdp _ 0 = []
bi sdp t n =
  let  ps_tail = bi sdp t (n - 1)
       p = bestExt sdp t ps_tail
   in  p : ps_tail

bi' :: (Show x, Ord x) => SDP x y -> Int -> Int -> PolicySeq x y
bi' sdp _ 0 = []
bi' sdp t n =
  let  ps_tail = bi' sdp t (n - 1)
       p = bestExt' sdp t ps_tail
   in  p : ps_tail

-- Optimal extension
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

best :: (Show x, Ord x) => SDP x y -> Int -> Int -> x -> (y, Val)
best sdp t n x
    | n == 0 = error "Horizon must be greater than zero!"
    | otherwise =
        let  ps  = bi sdp (t + 1) (n - 1)
             p   = bestExt sdp t ps
             b   = fromMaybe (error "No action found!") (Map.lookup x p)
             vb  = val sdp t (p : ps) x
        in (b, vb)

best' :: (Show x, Ord x) => SDP x y -> Int -> Int -> x -> (y, Val)
best' sdp t n x
    | n == 0 = error "Horizon must be greater than zero!"
    | otherwise =
        let  ps  = bi' sdp (t + 1) (n - 1)
             p   = bestExt' sdp t ps
             b   = fromMaybe (error "No action found!") (Map.lookup x p)
             vb  = val' sdp t (p : ps) x
        in (b, vb)

-- Compute how much a state matters for optimal decision-making
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