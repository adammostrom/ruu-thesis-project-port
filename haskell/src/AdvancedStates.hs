-- | Advanced states for the climate economy example
module AdvancedStates where

import SDPTypes(SDP(SDP), Val)
import Prob(Prob, mkSimpleProb)
import AdvancedProb(boxMullerTransform, myRandomGen, bucketWithProbs, cdfNorm)

-- Add these parameters at the top of the file
e, c :: Int
e = 5  -- economic dimension
c = 5  -- climate dimension

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

-- Let x2, x3 roam between 0 and e/c respectively
states :: Time -> [State]
states _ = [State x1 x2 x3 | x1 <- ['S', 'D'], x2 <- [0..e], x3 <- [0..c]]

-- | Get actions for a given time and state
actions :: Time -> State -> [Action]
actions _ _ = [MaxEcon, MaxClim, Passive]


-----------------------------------

-- Normalized product reward, like in Python
reward :: Time -> State -> Action -> State -> Val
reward _ _ _ (State _ x2' x3')
  | x2' > 0 && x3' > 0
  = fromIntegral (x2' * x3') / fromIntegral (e * c)
  | otherwise
  = 0
reward _ _ _ None = 0


next' :: Time -> State -> Action -> Prob State
next' _ None _ = error "No next state for None"
next' _ (State x1 x2 x3) act =
  -- Compute next probabilistic state by applying drift and Gaussian uncertainty
  let 
    sigma = 0.1
    -- Compute the probabilities of the next states
    statesWithWeights :: [(State, Double)]
    statesWithWeights =
      let econPairs = zip econEdges pEcon  -- [(e, pe)]
          climPairs = zip climEdges pClim  -- [(c, pc)]
      in [ (State x1' (round e') (round c'), pe * pc) | (e', pe) <- econPairs, (c', pc) <- climPairs ]
      where
        (econDrift, climDrift) = drifts act
        econMu = fromIntegral x2 + econDrift
        climMu = fromIntegral x3 + climDrift

        econEdges, climEdges :: [Double]
        econEdges = [-1/0] ++ map fromIntegral [0 .. e] ++ [1/0]
        climEdges = [-1/0] ++ map fromIntegral [0 .. c] ++ [1/0]

        pEcon = map (\x -> cdfNorm $ ((x - econMu) / sigma)) [0 .. fromIntegral e]
        pClim = map (\x -> cdfNorm $ ((x - climMu) / sigma)) [0 .. fromIntegral c]

        x1' = if act == MaxClim then 'S' else 'D'

  in mkSimpleProb statesWithWeights

