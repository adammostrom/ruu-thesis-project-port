module Theory where

-- import Rel.TotalPreorder

-- NOTE: Idris code marked in comments as beginning with ">".

import Control.Monad (liftM2)
import Probabilities

--------------------------------------------------------------------
-- Theory.hs
type Time = Int

type Policy = Map

type Log = String

type Val = Double -- Val is the type of rewards

data State = Nil | State deriving (Show, Eq, Ord)


data Control t x = Control State x deriving (Show)

-- Available controls: given a time & state, return a list of valid controls
availableControls ::  Time -> State -> [Control]


-- Reward function
reward :: X a => a -> Control a b -> a -> Val

-- Meas is a specification function defined by the case. Later we will set meas to compute expected value of 
meas :: [(a, Val)] -> Val -- Vettefan

--------------------------------------------------------------------
-- Inititalize states
DHU :: State
DHC :: State
DLU :: State
DLC :: State
SHU :: State
SHC :: State
SLU :: State
SLC :: State

Start :: Control
Delay :: Control
Unity :: Control

isCommitted :: State -> Bool
isCommitted x = True if x == DHC ||  x == DLC  ||  x == SHC ||  x == SLC else False

isDisrupted :: State -> Bool
isDisrupted x = True if x == DLU ||  x == DLC  ||  x == SLU ||  x == SLC else False

isStarted :: State -> Bool
isStarted x = True if x == SHU ||  x == SHC  ||  x == SLU ||  x == SLC else False


availableControls _ x = Unity if isStarted == True    -- Irreversibility of choice -> sink
availableControls t x = [Start, Delay]                -- For now ignore time.

reward :: Time -> State -> Control -> State -> Val
reward t x y x' = if (isCommitted x' || isDisrupted x') then 0 else 1
-- reward _ _ _ x' = if x' == DHU || x' == SHU then 1 
                else 0

mMeas :: Time -> State -> Control -> Double
mMeas t x Unity = 0
mMeas t x _ = 1 -- Placeholder

-- Measure the 
-- The value of a policy sequence is defined inductively as the measuee of an M structure of Val values
val :: [Control] -> Val
val [] = 0
val (p:ps) = meas p + val ps

next :: Time -> State -> Control -> [(State, Probability)]
next 0 DHU Start = normalize $ mkProbabilityList
  [ (DHU, pD_Start * pH_D_DH * pU_D_0),
    (DHC, pD_Start * pH_D_DH * pC_D_0),
    (DLU, pD_Start * pL_D_DH * pU_D_0),
    (DLC, pD_Start * pL_D_DH * pC_D_0),
    (SHU, pS_Start * pH_S_DH * pU_S_0),
    (SHC, pS_Start * pH_S_DH * pC_S_0),
    (SLU, pS_Start * pL_S_DH * pU_S_0),
    (SLC, pS_Start * pL_S_DH * pC_S_0)
  ]
next 0 DHU Delay = normalize $ mkProbabilityList
  [ (DHU, pD_Delay * pH_D_DH * pU_D_0),
    (DHC, pD_Delay * pH_D_DH * pC_D_0),
    (DLU, pD_Delay * pL_D_DH * pU_D_0),
    (DLC, pD_Delay * pL_D_DH * pC_D_0),
    (SHU, pS_Delay * pH_S_DH * pU_S_0),
    (SHC, pS_Delay * pH_S_DH * pC_S_0),
    (SLU, pS_Delay * pL_S_DH * pU_S_0),
    (SLC, pS_Delay * pL_S_DH * pC_S_0)
  ]
next 0 DHC Start = normalize $ mkProbabilityList
  [ (DHC, pD_Start * pH_D_DH),
    (DLC, pD_Start * pL_D_DH),
    (SHC, pS_Start * pH_S_DH),
    (SLC, pS_Start * pL_S_DH)
  ]
next 0 DHC Delay = normalize $ mkProbabilityList
  [ (DHC, pD_Delay * pH_D_DH),
    (DLC, pD_Delay * pL_D_DH),
    (SHC, pS_Delay * pH_S_DH),
    (SLC, pS_Delay * pL_S_DH)
  ]
next 0 DLU Start = normalize $ mkProbabilityList
  [ (DHU, pD_Start * pH_D_DL * pU_D_0),
    (DHC, pD_Start * pH_D_DL * pC_D_0),
    (DLU, pD_Start * pL_D_DL * pU_D_0),
    (DLC, pD_Start * pL_D_DL * pC_D_0),
    (SHU, pS_Start * pH_S_DL * pU_S_0),
    (SHC, pS_Start * pH_S_DL * pC_S_0),
    (SLU, pS_Start * pL_S_DL * pU_S_0),
    (SLC, pS_Start * pL_S_DL * pC_S_0)
  ]
next 0 DLU Delay = normalize $ mkProbabilityList
  [ (DHU, pD_Delay * pH_D_DL * pU_D_0),
    (DHC, pD_Delay * pH_D_DL * pC_D_0),
    (DLU, pD_Delay * pL_D_DL * pU_D_0),
    (DLC, pD_Delay * pL_D_DL * pC_D_0),
    (SHU, pS_Delay * pH_S_DL * pU_S_0),
    (SHC, pS_Delay * pH_S_DL * pC_S_0),
    (SLU, pS_Delay * pL_S_DL * pU_S_0),
    (SLC, pS_Delay * pL_S_DL * pC_S_0)
  ]
next 0 DLC Start = normalize $ mkProbabilityList
  [ (DHC, pD_Start * pH_D_DL),
    (DLC, pD_Start * pL_D_DL),
    (SHC, pS_Start * pH_S_DL),
    (SLC, pS_Start * pL_S_DL)
  ]
next 0 DLC Delay = normalize $ mkProbabilityList
  [ (DHC, pD_Delay * pH_D_DL),
    (DLC, pD_Delay * pL_D_DL),
    (SHC, pS_Delay * pH_S_DL),
    (SLC, pS_Delay * pL_S_DL)
  ]
next 0 SHU _ = normalize $ mkProbabilityList
  [ (SHU, pH_S_SH * pU_S_0),
    (SHC, pH_S_SH * pC_S_0),
    (SLU, pL_S_SH * pU_S_0),
    (SLC, pL_S_SH * pC_S_0)
  ]
next 0 SHC _ = normalize $ mkProbabilityList
  [ (SHC, pH_S_SH),
    (SLC, pL_S_SH)
  ]
next 0 SLU _ = normalize $ mkProbabilityList
  [ (SHU, pH_S_SL * pU_S_0),
    (SHC, pH_S_SL * pC_S_0),
    (SLU, pL_S_SL * pU_S_0),
    (SLC, pL_S_SL * pC_S_0)
  ]
next 0 SLC _ = normalize $ mkProbabilityList
  [ (SHC, pH_S_SL),
    (SLC, pL_S_SL)
  ]
next t DHU Start = normalize $ mkProbabilityList
  [ (DHU, pD_Start * pH_D_DH * pU_D),
    (DHC, pD_Start * pH_D_DH * pC_D),
    (DLU, pD_Start * pL_D_DH * pU_D),
    (DLC, pD_Start * pL_D_DH * pC_D),
    (SHU, pS_Start * pH_S_DH * pU_S),
    (SHC, pS_Start * pH_S_DH * pC_S),
    (SLU, pS_Start * pL_S_DH * pU_S),
    (SLC, pS_Start * pL_S_DH * pC_S)
  ]
next t DHU Delay = normalize $ mkProbabilityList
  [ (DHU, pD_Delay * pH_D_DH * pU_D),
    (DHC, pD_Delay * pH_D_DH * pC_D),
    (DLU, pD_Delay * pL_D_DH * pU_D),
    (DLC, pD_Delay * pL_D_DH * pC_D),
    (SHU, pS_Delay * pH_S_DH * pU_S),
    (SHC, pS_Delay * pH_S_DH * pC_S),
    (SLU, pS_Delay * pL_S_DH * pU_S),
    (SLC, pS_Delay * pL_S_DH * pC_S)
  ]
next t DHC Start = normalize $ mkProbabilityList
  [ (DHC, pD_Start * pH_D_DH),
    (DLC, pD_Start * pL_D_DH),
    (SHC, pS_Start * pH_S_DH),
    (SLC, pS_Start * pL_S_DH)
  ]
next t DHC Delay = normalize $ mkProbabilityList
  [ (DHC, pD_Delay * pH_D_DH),
    (DLC, pD_Delay * pL_D_DH),
    (SHC, pS_Delay * pH_S_DH),
    (SLC, pS_Delay * pL_S_DH)
  ]
next t DLU Start = normalize $ mkProbabilityList
  [ (DHU, pD_Start * pH_D_DL * pU_D),
    (DHC, pD_Start * pH_D_DL * pC_D),
    (DLU, pD_Start * pL_D_DL * pU_D),
    (DLC, pD_Start * pL_D_DL * pC_D),
    (SHU, pS_Start * pH_S_DL * pU_S),
    (SHC, pS_Start * pH_S_DL * pC_S),
    (SLU, pS_Start * pL_S_DL * pU_S),
    (SLC, pS_Start * pL_S_DL * pC_S)
  ]
next t DLU Delay = normalize $ mkProbabilityList
  [ (DHU, pD_Delay * pH_D_DL * pU_D),
    (DHC, pD_Delay * pH_D_DL * pC_D),
    (DLU, pD_Delay * pL_D_DL * pU_D),
    (DLC, pD_Delay * pL_D_DL * pC_D),
    (SHU, pS_Delay * pH_S_DL * pU_S),
    (SHC, pS_Delay * pH_S_DL * pC_S),
    (SLU, pS_Delay * pL_S_DL * pU_S),
    (SLC, pS_Delay * pL_S_DL * pC_S)
  ]
next t DLC Start = normalize $ mkProbabilityList
  [ (DHC, pD_Start * pH_D_DL),
    (DLC, pD_Start * pL_D_DL),
    (SHC, pS_Start * pH_S_DL),
    (SLC, pS_Start * pL_S_DL)
  ]
next t DLC Delay = normalize $ mkProbabilityList
  [ (DHC, pD_Delay * pH_D_DL),
    (DLC, pD_Delay * pL_D_DL),
    (SHC, pS_Delay * pH_S_DL),
    (SLC, pS_Delay * pL_S_DL)
  ]
next t SHU Unity = normalize $ mkProbabilityList
  [ (SHU, pH_S_SH * pU_S),
    (SHC, pH_S_SH * pC_S),
    (SLU, pL_S_SH * pU_S),
    (SLC, pL_S_SH * pC_S)
  ]
next t SHC Unity = normalize $ mkProbabilityList
  [ (SHC, pH_S_SH),
    (SLC, pL_S_SH)
  ]
next t SLU Unity = normalize $ mkProbabilityList
  [ (SHU, pH_S_SL * pU_S),
    (SHC, pH_S_SL * pC_S),
    (SLU, pL_S_SL * pU_S),
    (SLC, pL_S_SL * pC_S)
  ]
next t SLC Unity = normalize $ mkProbabilityList
  [ (SHC, pH_S_SL),
    (SLC, pL_S_SL)
  ]
next _ _ _ = error "Invalid state or action combination"


bi :: Time -> Int -> [Control]
bi _ 0 = []
bi t n = p:ps 
    where
        ps = bi t (pred n)
        p = bestExt t ps


bestExt :: 

best :: Time -> Int -> State -> Log
best _ 0 _ = "The horizon must be greater than zero!"
best t n x = let
    ps = bi (succ t) (succ n) in
        let p = bestExt ps in
            let b = p x in
                let vb = val (p:ps) in
                    "Horizon, best, value : " ++ Show(succ n) ++ ", " ++ Show(b) ++ ", " ++ Show(vb)