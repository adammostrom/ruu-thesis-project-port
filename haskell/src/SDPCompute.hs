module SDPCompute (module SDPCompute, module SDPTypes) where
import Data.List (maximumBy, minimumBy, nub)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Function (on)
import Data.Ord (comparing)
import Prob(Prob, runProb, expectation)
import SDPTypes

val :: (Ord x, Show x) => SDP x y -> Int -> PolicySeq x y -> x -> Val
val = map $ val' sdp t (p : ps) x []
  where 
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
bi sdp _ n | n <= 0 = [] -- Added safety from negative values mostly for testing /AM
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


optExt2 :: (Show x, Ord x) => (Val -> Val -> Ordering) -> SDP x y -> Int -> PolicySeq x y -> Policy x y
optExt2 cmp sdp t ps =
  let
    sts = states sdp t
    -- Memoize val for each state
    valMap :: Map x Val
    valMap = Map.fromList [ (state, val sdp t ps state) | state <- sts ]

    -- Use memoized version
    getVal state = valMap Map.! state

    optAction t state ps =
      let actionsForState = actions sdp t state
          actionVals = [ (a, getVal state) | a <- actionsForState ]
          best = maximumBy (cmp `on` snd) actionVals
      in (state, fst best)

  in Map.fromList (map (\state -> optAction t state ps) sts)









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
