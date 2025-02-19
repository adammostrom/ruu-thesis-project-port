module Combined where

import Data.List (maximumBy)
import Data.Ord (comparing)
import CustomMap
    ( CustomMap, lookupC, fromList, toList, findWithDefault )

-- Define State and Action types
data State = DHU | DHC | DLU | DLC | SHU | SHC | SLU | SLC
  deriving (Show, Eq, Enum)

data Action = Start | Delay | Unit
    deriving (Show, Eq, Enum)

type Probability = Double

type Policy = CustomMap State Action

type Transition = CustomMap State Probability

type Time = Int

type Horizon = Int

type Policies = [Policy]

pS_Start :: Double
pS_Start = 0.9

pD_Start :: Double
pD_Start = (1.0 - pS_Start)

pD_Delay :: Double
pD_Delay = 0.9

pS_Delay :: Double
pS_Delay = (1.0 - pD_Delay)

pL_S_DH :: Double
pL_S_DH = 0.7

pL_S_DL :: Double
pL_S_DL = 0.9

pL_S_SL :: Double
pL_S_SL = 0.7

pL_S_SH :: Double
pL_S_SH = 0.3

pL_D_DH :: Double
pL_D_DH = pL_S_SH

pL_D_DL :: Double
pL_D_DL = (pL_S_SL)

pU_S_0 :: Double
pU_S_0 = 0.9

pU_D_0 :: Double
pU_D_0 = 0.7

pU_S :: Double
pU_S = 0.9

pU_D :: Double
pU_D = 0.3

pH_S_DH :: Double
pH_S_DH = (1.0 - pL_S_DH)

pH_S_SH :: Double
pH_S_SH = (1.0 - pL_S_SH)

pH_S_DL :: Double
pH_S_DL = (1.0 - pL_S_DL)

pH_S_SL :: Double
pH_S_SL = (1.0 - pL_S_SL)

pH_D_DH :: Double
pH_D_DH = (1.0 - pL_D_DH)

pH_D_DL :: Double
pH_D_DL = (1.0 - pL_D_DL)

pC_S_0 :: Double
pC_S_0 = (1.0 - pU_S_0)

pC_D_0 :: Double
pC_D_0 = (1.0 - pU_D_0)

pC_S :: Double
pC_S = (1.0 - pU_S)

pC_D :: Double
pC_D = (1.0 - pU_D)

-- List of states (fixed)
states :: [State]
states = [DHU, DHC, DLU, DLC, SHU, SHC, SLU, SLC]

-- Actions based on state
actions :: State -> [Action]
actions s
  | s `elem` [DHU, DHC, DLU, DLC] = [Start, Delay]
  | otherwise = []

-- Reward function (returns 1 for "SHU", else 0)
reward :: Time -> State -> Action -> State -> Int
reward _ x _ _ = if x == SHU then 1 else 0

-- Value function (calculates value based on policies)
val :: Time -> Horizon -> Policies -> State -> Double
val _ _ [] _ = 0
val t n (p : ps) x =
  case lookupC x p of
    Nothing -> 0
    Just y -> sum [(fromIntegral (reward t x y x') + val (t + 1) (n - 1) ps x') * pr | (x', pr) <- toList (nextFun t x y)]

-- Best extension function (computes best action for a given state)
bestExt :: Time -> Horizon -> Policies -> Policy
bestExt t n psTail = fromList $ map bestAction states
  where
    bestAction state
      | null (actions state) = (state, Unit)
      | otherwise = (state, snd $ maximumBy (comparing fst) [(val t n psTail state, a) | a <- actions state])

-- Bi function (builds a policy sequence recursively)
bi :: Time -> Horizon -> Policies
bi _ 0 = []
bi t n =
  let psTail = bi (t + 1) (n - 1)
      p = bestExt t n psTail
   in p : psTail

-- Best action for a given state
best :: Time -> Horizon -> State -> (Action, Double)
best t n x =
  let ps = bi (t + 1) n
      p = bestExt t n ps
      b = findWithDefault Unit x p
      vb = val t n (p : ps) x
   in (b, vb)

-- Placeholder for next state transitions (to be filled with actual transition probabilities)
-- Transition function using CustomMap
nextFun :: Time -> State -> Action -> Transition
nextFun _ DHU Start =
  fromList
    [ (DHU, pD_Start * pH_D_DH * pU_D_0),
      (DHC, pD_Start * pH_D_DH * pC_D_0),
      (DLU, pD_Start * pL_D_DH * pU_D_0),
      (DLC, pD_Start * pL_D_DH * pC_D_0),
      (SHU, pS_Start * pH_S_DH * pU_S_0),
      (SHC, pS_Start * pH_S_DH * pC_S_0),
      (SLU, pS_Start * pL_S_DH * pU_S_0),
      (SLC, pS_Start * pL_S_DH * pC_S_0)
    ]
nextFun _ DHU Delay =
  fromList
    [ (DHU, pD_Delay * pH_D_DH * pU_D_0),
      (DHC, pD_Delay * pH_D_DH * pC_D_0),
      (DLU, pD_Delay * pL_D_DH * pU_D_0),
      (DLC, pD_Delay * pL_D_DH * pC_D_0),
      (SHU, pS_Delay * pH_S_DH * pU_S_0),
      (SHC, pS_Delay * pH_S_DH * pC_S_0),
      (SLU, pS_Delay * pL_S_DH * pU_S_0),
      (SLC, pS_Delay * pL_S_DH * pC_S_0)
    ]
nextFun _ _ _ = fromList [] -- Default case: empty transition map
