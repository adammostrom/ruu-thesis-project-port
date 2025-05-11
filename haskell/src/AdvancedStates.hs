-- Advanced states for the climate economy example
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

data State = State Action Int Int | None
  deriving (Show, Eq, Ord, Read)

-- Drifts as a function of action
drifts :: Action -> (Val, Val)
drifts action =
  case action of
    MaxEcon -> (1.0, 0.4)
    MaxClim -> (-0.4, 1.0)
    Passive -> (0, 0)

advcase :: SDP State Action
advcase = SDP reward next actions states

-- Let x2, x3 roam between 1 and e/c respectively
states :: Time -> [State]
states _ = [State x1 x2 x3 | x1 <- [MaxEcon, MaxClim, Passive], x2 <- [1 .. e], x3 <- [1 .. c]]

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


next :: Time -> State -> Action -> Prob State
next _ None _ = error "No next state for None"
next _ (State x1 x2 x3) act =
  -- Compute next probabilistic state by applying drift and Gaussian uncertainty
  let 
    sigma = 0.5
    -- Compute the probabilities of the next states
    statesWithWeights :: [(State, Double)]
    statesWithWeights =
      let econPairs = zip econEdges pEcon  -- [(e, pe)]
          climPairs = zip climEdges pClim  -- [(c, pc)]
          -- Helper function to clamp values to [1..maxVal]
          clamp :: Int -> Double -> Int
          clamp maxVal x = max 1 (min maxVal (round x))
      in [ (State x1' (clamp e e') (clamp c c'), pe * pc) | (e', pe) <- econPairs, (c', pc) <- climPairs ]
      where
        x1' = act

        (econDrift, climDrift) = drifts act
        econMu = fromIntegral x2 + econDrift
        climMu = fromIntegral x3 + climDrift

        econEdges, climEdges :: [Double]
        -- Include infinities to capture full probability mass
        econEdges = (-1/0) : map fromIntegral [1 .. e] ++ [1/0]
        climEdges = (-1/0) : map fromIntegral [1 .. c] ++ [1/0]

        -- Calculate probabilities by taking differences of consecutive CDF values
        -- This ensures we capture all probability mass between -∞ and +∞
        pEcon = zipWith (-) 
            (map (\x -> cdfNorm ((x - econMu) / sigma)) (tail econEdges))
            (map (\x -> cdfNorm ((x - econMu) / sigma)) econEdges)

        pClim = zipWith (-) 
            (map (\x -> cdfNorm ((x - climMu) / sigma)) (tail climEdges))
            (map (\x -> cdfNorm ((x - climMu) / sigma)) climEdges)


  in mkSimpleProb statesWithWeights

