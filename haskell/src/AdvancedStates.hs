-- | Advanced states for the climate economy example
module AdvancedStates where

import SDPTypes(SDP(SDP), Val)
import Prob(Prob, mkSimpleProb)
import AdvancedProb(boxMullerTransform, myRandomGen, bucketWithProbs)

type Time = Int

data Action = MaxEcon | MaxClim | Passive
    deriving (Show, Eq, Enum, Ord, Read)

data State = State Char Int Int | None
  deriving (Show, Eq, Ord, Read)

-- Drifts as a function of action
drifts :: Action -> (Val, Val)
drifts action =
  case action of
    MaxEcon -> (-0.4, 1.0)
    MaxClim -> (1.0, 0.4)
    Passive -> (0, 0)

advcase :: SDP State Action
advcase = SDP reward next' actions states

-- Let x2, x3 roam between 0 and 5.
getStates :: [State]
getStates = [State x1 x2 x3 | x1 <- ['S', 'D'], x2 <- [0..5], x3 <- [0..5]]

-- Get states for a given time as per the paper
states :: Time -> [State]
states _ = getStates

-- | Get actions for a given time and state
actions :: Time -> State -> [Action]
actions _ _ = [MaxEcon, MaxClim, Passive]

-- For normalization of rewards: --
maxX2 :: Int
maxX2 = maximum [ i | State _ i _ <- getStates ]

maxX3 :: Int
maxX3 = maximum [ j | State _ _ j <- getStates ]
-----------------------------------

-- Normalized product reward, like in Python
reward :: Time -> State -> Action -> State -> Val
reward _ _ _ (State _ x2' x3')
  | x2' > 0 && x3' > 0
  = fromIntegral (x2' * x3') / fromIntegral (maxX2 * maxX3)
  | otherwise
  = 0
reward _ _ _ None = 0


next' :: Time -> State -> Action -> Prob State
next' _ None _ = error "No next state for None"
next' _ (State x1 x2 x3) act =
  -- Compute next probabilistic state by applying drift and Gaussian uncertainty
  let sigma = 0.1
      -- The drift is a function of the action
      (econDrift, climDrift) = drifts act
      -- Compute the mean of the normal distribution of the samples
      econMu = fromIntegral x2 + econDrift
      climMu = fromIntegral x3 + climDrift

      -- Generate a pure random stream
      randomStream = myRandomGen  -- Assume it's a fixed [Double]

      -- Generate samples using Box-Muller (pure version)
      normalSamples = boxMullerTransform randomStream
      -- Generate samples for the economic and climate variables
      econSamples = map (\z -> econMu + sigma * z) $ take 100 normalSamples
      climSamples = map (\z -> climMu + sigma * z) $ drop 100 normalSamples

      -- Group the samples into buckets and compute the probabilities
      econBuckets = bucketWithProbs 5 econSamples
      climBuckets = bucketWithProbs 5 climSamples

      -- Determine the next state based on the action
      x1' = if act == MaxClim then 'S' else 'D'

      -- Compute the probabilities of the next states
      statesWithWeights = [ (State x1' e c, pe * pc)
                          | (e, pe) <- econBuckets
                          , (c, pc) <- climBuckets ]

  in mkSimpleProb statesWithWeights

