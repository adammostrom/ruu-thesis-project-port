module Theory where

-- import Rel.TotalPreorder

-- NOTE: Idris code marked in comments as beginning with ">".

import Control.Monad (liftM2)
import Probabilities

--------------------------------------------------------------------
-- Theory.hs
type Time = Int

type Policy = Map



data State = Nil | State deriving (Show, Eq, Ord)


data Control t x = Control State x deriving (Show)

-- Available controls: given a time & state, return a list of valid controls
availableControls ::  Time -> State -> [Control]


-- Reward function
reward :: X a => a -> Control a b -> a -> Float

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
reward _ _ _ x' = if x' == DHU || x' == SHU then 1 
                else 0


next :: Time -> State -> Control -> Prob [(State, Probability)]
next 0 DHU Start = mkProbabilityList
  [ (DHU, pD_Start * pH_D_DH * pU_D_0),
    (DHC, pD_Start * pH_D_DH * pC_D_0),
    (DLU, pD_Start * pL_D_DH * pU_D_0),
    (DLC, pD_Start * pL_D_DH * pC_D_0),
    (SHU, pS_Start * pH_S_DH * pU_S_0),
    (SHC, pS_Start * pH_S_DH * pC_S_0),
    (SLU, pS_Start * pL_S_DH * pU_S_0),
    (SLC, pS_Start * pL_S_DH * pC_S_0)
  ]
next 0 DHU Delay = mkProbabilityList
  [ (DHU, pD_Delay * pH_D_DH * pU_D_0),
    (DHC, pD_Delay * pH_D_DH * pC_D_0),
    (DLU, pD_Delay * pL_D_DH * pU_D_0),
    (DLC, pD_Delay * pL_D_DH * pC_D_0),
    (SHU, pS_Delay * pH_S_DH * pU_S_0),
    (SHC, pS_Delay * pH_S_DH * pC_S_0),
    (SLU, pS_Delay * pL_S_DH * pU_S_0),
    (SLC, pS_Delay * pL_S_DH * pC_S_0)
  ]
next 0 DHC Start = mkProbabilityList
  [ (DHC, pD_Start * pH_D_DH),
    (DLC, pD_Start * pL_D_DH),
    (SHC, pS_Start * pH_S_DH),
    (SLC, pS_Start * pL_S_DH)
  ]
next 0 DHC Delay = mkProbabilityList
  [ (DHC, pD_Delay * pH_D_DH),
    (DLC, pD_Delay * pL_D_DH),
    (SHC, pS_Delay * pH_S_DH),
    (SLC, pS_Delay * pL_S_DH)
  ]
next 0 DLU Start = mkProbabilityList
  [ (DHU, pD_Start * pH_D_DL * pU_D_0),
    (DHC, pD_Start * pH_D_DL * pC_D_0),
    (DLU, pD_Start * pL_D_DL * pU_D_0),
    (DLC, pD_Start * pL_D_DL * pC_D_0),
    (SHU, pS_Start * pH_S_DL * pU_S_0),
    (SHC, pS_Start * pH_S_DL * pC_S_0),
    (SLU, pS_Start * pL_S_DL * pU_S_0),
    (SLC, pS_Start * pL_S_DL * pC_S_0)
  ]
next 0 DLU Delay = mkProbabilityList
  [ (DHU, pD_Delay * pH_D_DL * pU_D_0),
    (DHC, pD_Delay * pH_D_DL * pC_D_0),
    (DLU, pD_Delay * pL_D_DL * pU_D_0),
    (DLC, pD_Delay * pL_D_DL * pC_D_0),
    (SHU, pS_Delay * pH_S_DL * pU_S_0),
    (SHC, pS_Delay * pH_S_DL * pC_S_0),
    (SLU, pS_Delay * pL_S_DL * pU_S_0),
    (SLC, pS_Delay * pL_S_DL * pC_S_0)
  ]
next 0 DLC Start = mkProbabilityList
  [ (DHC, pD_Start * pH_D_DL),
    (DLC, pD_Start * pL_D_DL),
    (SHC, pS_Start * pH_S_DL),
    (SLC, pS_Start * pL_S_DL)
  ]
next 0 DLC Delay = mkProbabilityList
  [ (DHC, pD_Delay * pH_D_DL),
    (DLC, pD_Delay * pL_D_DL),
    (SHC, pS_Delay * pH_S_DL),
    (SLC, pS_Delay * pL_S_DL)
  ]
next 0 SHU _ = mkProbabilityList
  [ (SHU, pH_S_SH * pU_S_0),
    (SHC, pH_S_SH * pC_S_0),
    (SLU, pL_S_SH * pU_S_0),
    (SLC, pL_S_SH * pC_S_0)
  ]
next 0 SHC _ = mkProbabilityList
  [ (SHC, pH_S_SH),
    (SLC, pL_S_SH)
  ]
next 0 SLU _ = mkProbabilityList
  [ (SHU, pH_S_SL * pU_S_0),
    (SHC, pH_S_SL * pC_S_0),
    (SLU, pL_S_SL * pU_S_0),
    (SLC, pL_S_SL * pC_S_0)
  ]
next 0 SLC _ = mkProbabilityList
  [ (SHC, pH_S_SL),
    (SLC, pL_S_SL)
  ]
next t DHU Start = mkProbabilityList
  [ (DHU, pD_Start * pH_D_DH * pU_D),
    (DHC, pD_Start * pH_D_DH * pC_D),
    (DLU, pD_Start * pL_D_DH * pU_D),
    (DLC, pD_Start * pL_D_DH * pC_D),
    (SHU, pS_Start * pH_S_DH * pU_S),
    (SHC, pS_Start * pH_S_DH * pC_S),
    (SLU, pS_Start * pL_S_DH * pU_S),
    (SLC, pS_Start * pL_S_DH * pC_S)
  ]
next t DHU Delay = mkProbabilityList
  [ (DHU, pD_Delay * pH_D_DH * pU_D),
    (DHC, pD_Delay * pH_D_DH * pC_D),
    (DLU, pD_Delay * pL_D_DH * pU_D),
    (DLC, pD_Delay * pL_D_DH * pC_D),
    (SHU, pS_Delay * pH_S_DH * pU_S),
    (SHC, pS_Delay * pH_S_DH * pC_S),
    (SLU, pS_Delay * pL_S_DH * pU_S),
    (SLC, pS_Delay * pL_S_DH * pC_S)
  ]
next t DHC Start = mkProbabilityList
  [ (DHC, pD_Start * pH_D_DH),
    (DLC, pD_Start * pL_D_DH),
    (SHC, pS_Start * pH_S_DH),
    (SLC, pS_Start * pL_S_DH)
  ]
next t DHC Delay = mkProbabilityList
  [ (DHC, pD_Delay * pH_D_DH),
    (DLC, pD_Delay * pL_D_DH),
    (SHC, pS_Delay * pH_S_DH),
    (SLC, pS_Delay * pL_S_DH)
  ]
next t DLU Start = mkProbabilityList
  [ (DHU, pD_Start * pH_D_DL * pU_D),
    (DHC, pD_Start * pH_D_DL * pC_D),
    (DLU, pD_Start * pL_D_DL * pU_D),
    (DLC, pD_Start * pL_D_DL * pC_D),
    (SHU, pS_Start * pH_S_DL * pU_S),
    (SHC, pS_Start * pH_S_DL * pC_S),
    (SLU, pS_Start * pL_S_DL * pU_S),
    (SLC, pS_Start * pL_S_DL * pC_S)
  ]
next t DLU Delay = mkProbabilityList
  [ (DHU, pD_Delay * pH_D_DL * pU_D),
    (DHC, pD_Delay * pH_D_DL * pC_D),
    (DLU, pD_Delay * pL_D_DL * pU_D),
    (DLC, pD_Delay * pL_D_DL * pC_D),
    (SHU, pS_Delay * pH_S_DL * pU_S),
    (SHC, pS_Delay * pH_S_DL * pC_S),
    (SLU, pS_Delay * pL_S_DL * pU_S),
    (SLC, pS_Delay * pL_S_DL * pC_S)
  ]
next t DLC Start = mkProbabilityList
  [ (DHC, pD_Start * pH_D_DL),
    (DLC, pD_Start * pL_D_DL),
    (SHC, pS_Start * pH_S_DL),
    (SLC, pS_Start * pL_S_DL)
  ]
next t DLC Delay = mkProbabilityList
  [ (DHC, pD_Delay * pH_D_DL),
    (DLC, pD_Delay * pL_D_DL),
    (SHC, pS_Delay * pH_S_DL),
    (SLC, pS_Delay * pL_S_DL)
  ]
next t SHU Unity = mkProbabilityList
  [ (SHU, pH_S_SH * pU_S),
    (SHC, pH_S_SH * pC_S),
    (SLU, pL_S_SH * pU_S),
    (SLC, pL_S_SH * pC_S)
  ]
next t SHC Unity = mkProbabilityList
  [ (SHC, pH_S_SH),
    (SLC, pL_S_SH)
  ]
next t SLU Unity = mkProbabilityList
  [ (SHU, pH_S_SL * pU_S),
    (SHC, pH_S_SL * pC_S),
    (SLU, pL_S_SL * pU_S),
    (SLC, pL_S_SL * pC_S)
  ]
next t SLC Unity = mkProbabilityList
  [ (SHC, pH_S_SL),
    (SLC, pL_S_SL)
  ]
next _ _ _ = error "Invalid state or action combination"

type Network = [Prob [State]]

bi :: RewardFnc -> Network -> (Reward, [State])
bi reward network = foldr network combine leafValue network
    where
        leafValue :: Network -> (Reward, )
