module GHGCase where

import GHGCaseParam
import Prob (Prob, mkSimpleProb)
import SDPTypes (SDP (SDP), Val)

-- Concrete Implementation of GHG case from the MatterMost paper

data Action = Start | Delay | Unit
  deriving (Show, Eq, Enum, Ord, Read)

data State = DHU | DHC | DLU | DLC | SHU | SHC | SLU | SLC
  deriving (Show, Eq, Enum, Ord, Read)

ghgcase :: SDP State Action
ghgcase = SDP reward next actions states

-- TODO make a version with a few probability parameters

getStates :: [State]
getStates = [DHU, DHC, DLU, DLC, SHU, SHC, SLU, SLC]

states :: Int -> [State]
states _ = getStates

actions :: Int -> State -> [Action]
actions _ x
  | x `elem` [DHU, DHC, DLU, DLC] = [Start, Delay]
  | x `elem` [SHU, SHC, SLU, SLC] = [Unit]
  | otherwise = error "no legitimate states"

reward :: Int -> State -> Action -> State -> Val
reward _ _ _ next_x = if next_x == DHU || next_x == SHU then 1 else 0

next :: Int -> State -> Action -> Prob State
next t x y = case x of
  DHU -> nextDU t y pH_D_DH pL_D_DH pH_S_DH pL_S_DH
  DLU -> nextDU t y pH_D_DL pL_D_DL pH_S_DL pL_S_DL
  DHC -> nextDC t y pH_D_DH pL_D_DH pH_S_DH pL_S_DH
  DLC -> nextDC t y pH_D_DH pL_D_DH pH_S_DH pL_S_DH
  SHU -> nextSU t pH_S_SH pL_S_SH
  SLU -> nextSU t pH_S_SL pL_S_SL
  SHC -> nextSC pH_S_SH pL_S_SH
  SLC -> nextSC pH_S_SL pL_S_SL
  _ -> error "Invalid state or action combination"

nextDU :: Int -> Action -> Double -> Double -> Double -> Double -> Prob State
nextDU t y phd pld phs pls =
  mkSimpleProb
    [ (DHU, fst action * phd * nextCheckTime t DHU),
      (DHC, fst action * phd * nextCheckTime t DHC),
      (DLU, fst action * pld * nextCheckTime t DLU),
      (DLC, fst action * pld * nextCheckTime t DLC),
      (SHU, snd action * phs * nextCheckTime t SHU),
      (SHC, snd action * phs * nextCheckTime t SHC),
      (SLU, snd action * pls * nextCheckTime t SLU),
      (SLC, snd action * pls * nextCheckTime t SLC)
    ]
  where
    action = checkAction y

nextDC :: Int -> Action -> Double -> Double -> Double -> Double -> Prob State
nextDC t y phd pld phs pls =
  mkSimpleProb
    [ (DHC, fst action * phd),
      (DLC, fst action * pld),
      (SHC, snd action * phs),
      (SLC, snd action * pls)
    ]
  where
    action = checkAction y

nextSU :: Int -> Double -> Double -> Prob State
nextSU t phs pls =
  mkSimpleProb
    [ (SHU, phs * nextCheckTime t SHU),
      (SHC, phs * nextCheckTime t SHC),
      (SLU, pls * nextCheckTime t SLU),
      (SLC, pls * nextCheckTime t SLC)
    ]

nextSC :: Double -> Double -> Prob State
nextSC phs pls =
  mkSimpleProb
    [ (SHC, pH_S_SL),
      (SLC, pL_S_SL)
    ]

nextTime :: State -> Double
nextTime state = checkState state pU_D pC_D pU_S pC_S

nextTimeZero :: State -> Double
nextTimeZero state = checkState state pU_D_0 pC_D_0 pU_S_0 pC_S_0

nextCheckTime :: Int -> State -> Double
nextCheckTime t
  | t < 0 = error "Time cannot be negative"
  | t == 0 = nextTimeZero
  | otherwise = nextTime

checkAction :: Action -> (Double, Double)
checkAction y
  | y == Start = (pD_Start, pS_Start)
  | y == Delay = (pD_Delay, pS_Delay)
  | otherwise = error "Not a valid action"

checkState :: State -> Double -> Double -> Double -> Double -> Double
checkState state pud pcd pus pcs = case state of
  DHU -> pud
  DHC -> pcd
  DLU -> pud
  DLC -> pcd
  SHU -> pus
  SHC -> pcs
  SLU -> pus
  SLC -> pcs
