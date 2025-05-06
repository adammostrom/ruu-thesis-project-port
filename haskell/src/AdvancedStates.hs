-- | Advanced states for the climate economy example
module AdvancedStates where

import SDPTypes(SDP(SDP), Val)
import Prob(Prob, mkSimpleProb)
import AdvancedProb(boxMullerTransform, myRandomGen, bucketWithProbs)

-- | Time type
type Time = Int

-- | Action type: MaxEcon, MaxClim, Passive
data Action = MaxEcon | MaxClim | Passive
    deriving (Show, Eq, Enum, Ord, Read)

-- | State type: State Char Int Int | None
data State = State Char Int Int | None
  deriving (Show, Eq, Ord, Read)

-- | Drifts as a function of action
drifts :: Action -> (Val, Val)
drifts action =
  case action of
    MaxEcon -> (-0.4, 1.0)
    MaxClim -> (1.0, 0.4)
    Passive -> (0, 0)

-- | Climate economy SDP
ghgcase :: SDP State Action
ghgcase = SDP reward next' actions states

-- | Get all states
getStates :: [State]
getStates = [State x1 x2 x3 | x1 <- ['S', 'D'], x2 <- [0..5], x3 <- [0..5]]

-- | Get states for a given time
states :: Time -> [State]
states _ = getStates

-- | Get actions for a given time and state
actions :: Time -> State -> [Action]
actions _ _ = [MaxEcon, MaxClim, Passive]


maxX2 :: Int
maxX2 = maximum [ i | State _ i _ <- getStates ]

maxX3 :: Int
maxX3 = maximum [ j | State _ _ j <- getStates ]

-- | Normalized product reward
reward :: Time -> State -> Action -> State -> Val
reward _ _ _ (State _ x2' x3')
  | x2' > 0 && x3' > 0
  = fromIntegral (x2' * x3') / fromIntegral (maxX2 * maxX3)
  | otherwise
  = 0
reward _ _ _ None = 0


-- | Compute next probabilistic state by applying drift and Gaussian noise
next' :: Time -> State -> Action -> Prob State
next' _ None _ = error "No next state for None"
next' _ (State x1 x2 x3) act =
  let sigma = 0.1
      (econDrift, climDrift) = drifts act
      econMu = fromIntegral x2 + econDrift
      climMu = fromIntegral x3 + climDrift

      -- Pure random stream
      randomStream = myRandomGen  -- Assume it's a fixed [Double]

      -- Generate samples using Box-Muller (pure version)
      normalSamples = boxMullerTransform randomStream
      econSamples = map (\z -> econMu + sigma * z) $ take 100 normalSamples
      climSamples = map (\z -> climMu + sigma * z) $ drop 100 normalSamples

      econBuckets = bucketWithProbs 5 econSamples
      climBuckets = bucketWithProbs 5 climSamples

      x1' = if act == MaxClim then 'D' else 'S'

      statesWithWeights = [ (State x1' e c, pe * pc)
                          | (e, pe) <- econBuckets
                          , (c, pc) <- climBuckets ]

  in mkSimpleProb statesWithWeights

