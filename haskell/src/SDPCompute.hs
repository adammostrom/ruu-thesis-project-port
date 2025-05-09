module SDPCompute (module SDPCompute, module SDPTypes) where
import Data.List (maximumBy, minimumBy, nub)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Function (on)
import Data.Ord (comparing)
import Prob(Prob, runProb, expectation)
import SDPTypes
import Data.Map qualified as MemoMap  -- Add this to your imports

-- Type alias for our memoization cache key
type MemoKey x y = (Int, [Map x y], x)
type MemoCache x y = Map.Map (MemoKey x y) Val

val' :: (Ord x, Show x, Ord y) => SDP x y -> Int -> PolicySeq x y -> x -> Val
val' sdp = go sdp Map.empty
  where
    go :: (Ord x, Show x, Ord y) => SDP x y -> MemoCache x y -> Int -> PolicySeq x y -> x -> Val
    go _ _ _ [] _ = 0
    go sdp cache t (p : ps) x =
      let key = (t, p : ps, x)
      in case Map.lookup key cache of
           Just v -> v
           Nothing ->
             case Map.lookup x p of
               Nothing -> error $ "No action found in policy for state: " ++ show x
               Just y ->
                 let mNext = next sdp t x y
                     newCache = Map.insert key result cache  -- Create updated cache
                     recCall x' = reward sdp t x y x' + go sdp newCache (t + 1) ps x'
                     result = expectation recCall mNext
                 in result

val :: (Ord x, Show x) => SDP x y -> Int -> PolicySeq x y -> x -> Val
val sdp _ [] _ = 0
val sdp t (p : ps) x =
  case Map.lookup x p of
    Nothing -> error $ "No action found in policy for state: " ++ show x
    Just y ->
      let mNext = next sdp t x y
          recCall x' = reward sdp t x y x' + val sdp (t + 1) ps x'
      in expectation recCall mNext

-- Backwards induction
bi :: (Show x, Ord x) => SDP x y -> Int -> Int -> PolicySeq x y
bi sdp _ 0 = []
bi sdp t n =
  let  ps_tail = bi sdp t (n - 1)
       p = bestExt sdp t ps_tail
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
  -- TODO (optional) reuse the computation of val for ps (now
  -- potentially recomputed many times). This should be possible using
  -- memoization.

bestExt :: (Show x, Ord x) => SDP x y -> Int -> PolicySeq x y -> Policy x y
bestExt = optExt compare

flipOrder :: Ordering -> Ordering
flipOrder GT = LT
flipOrder LT = GT
flipOrder EQ = EQ

worstExt :: (Show x, Ord x) => SDP x y -> Int -> PolicySeq x y -> Policy x y
worstExt = optExt (\x y -> flipOrder (compare x y))

best :: (Show x, Ord x) => SDP x y -> Int -> Int -> x -> (y, Val)
best sdp t n x
    | n == 0 = error "Horizon must be greater than zero!"
    | otherwise =
        let  ps  = bi sdp (t + 1) (n - 1)
             p   = bestExt sdp t ps
             b   = fromMaybe (error "No action found!") (Map.lookup x p)
             vb  = val sdp t (p : ps) x
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

-- Memoized versions of functions using val'

-- Backwards induction with memoization
bi' :: (Show x, Ord x, Ord y) => SDP x y -> Int -> Int -> PolicySeq x y
bi' sdp _ 0 = []
bi' sdp t n =
  let ps_tail = bi' sdp t (n - 1)
      p = bestExt' sdp t ps_tail
  in p : ps_tail

-- Optimal extension with memoization
optExt' :: (Show x, Ord x, Ord y) => (Val->Val->Ordering) -> SDP x y -> Int -> PolicySeq x y -> Policy x y
optExt' cmp sdp t ps = Map.fromList $ map optAction (states sdp t)
  where
    optAction state = (state, getOptAction t state ps)
    getOptAction t state ps =
      let actionsForState = actions sdp t state
          actionValues = [ (action, val' sdp t (Map.singleton state action : ps) state)
                        | action <- actionsForState
                        ]
          opt = maximumBy (cmp `on` snd) actionValues
      in fst opt

-- Best extension with memoization
bestExt' :: (Show x, Ord x, Ord y) => SDP x y -> Int -> PolicySeq x y -> Policy x y
bestExt' = optExt' compare

-- Worst extension with memoization
worstExt' :: (Show x, Ord x, Ord y) => SDP x y -> Int -> PolicySeq x y -> Policy x y
worstExt' = optExt' (\x y -> flipOrder (compare x y))

-- Best action and value with memoization
best' :: (Show x, Ord x, Ord y) => SDP x y -> Int -> Int -> x -> (y, Val)
best' sdp t n x
    | n == 0 = error "Horizon must be greater than zero!"
    | otherwise =
        let ps = bi' sdp (t + 1) (n - 1)
            p = bestExt' sdp t ps
            b = fromMaybe (error "No action found!") (Map.lookup x p)
            vb = val' sdp t (p : ps) x
        in (b, vb)

-- Measure function with memoization
mMeas' :: (Show x, Ord x, Ord y) => SDP x y -> Int -> Int -> x -> Double
mMeas' sdp t n x =
      let ps = bi' sdp t n
          bestVal = val' sdp t ps x
          psTail = tail ps
          worstPolicy | null ps = error "mMeas needs at least one step but got an empty PolicySeq"
                      | otherwise = worstExt' sdp t psTail
          worstVal = val' sdp t (worstPolicy : psTail) x
      in if bestVal == 0 && worstVal == 0 then 0
         else (bestVal - worstVal) / bestVal
