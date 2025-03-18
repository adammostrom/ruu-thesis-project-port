module Specification where

import Theory


data Action = Start | Delay | Unit
  deriving (Show, Eq, Enum, Ord)

data State = DHU | DHC | DLU | DLC | SHU | SHC | SLU | SLC
  deriving (Show, Eq, Enum, Ord)


-- Helper: Ensure probabilities are non-negative
mkSimpleProb :: [(State, Double)] -> Prob State
mkSimpleProb = Prob . filter (\(_, p) -> p >= 0)

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


-- The next function imolemented as cases
next :: Int -> State -> Action -> Prob State
next t x y = case (t, x, y) of
  (0, DHU, Start) ->
    mkSimpleProb
      [ (DHU, pD_Start * pH_D_DH * pU_D_0),
        (DHC, pD_Start * pH_D_DH * pC_D_0),
        (DLU, pD_Start * pL_D_DH * pU_D_0),
        (DLC, pD_Start * pL_D_DH * pC_D_0),
        (SHU, pS_Start * pH_S_DH * pU_S_0),
        (SHC, pS_Start * pH_S_DH * pC_S_0),
        (SLU, pS_Start * pL_S_DH * pU_S_0),
        (SLC, pS_Start * pL_S_DH * pC_S_0)
      ]
  (0, DHU, Delay) ->
    mkSimpleProb
      [ (DHU, pD_Delay * pH_D_DH * pU_D_0),
        (DHC, pD_Delay * pH_D_DH * pC_D_0),
        (DLU, pD_Delay * pL_D_DH * pU_D_0),
        (DLC, pD_Delay * pL_D_DH * pC_D_0),
        (SHU, pS_Delay * pH_S_DH * pU_S_0),
        (SHC, pS_Delay * pH_S_DH * pC_S_0),
        (SLU, pS_Delay * pL_S_DH * pU_S_0),
        (SLC, pS_Delay * pL_S_DH * pC_S_0)
      ]
  (0, DHC, Start) ->
    mkSimpleProb
      [ (DHC, pD_Start * pH_D_DH),
        (DLC, pD_Start * pL_D_DH),
        (SHC, pS_Start * pH_S_DH),
        (SLC, pS_Start * pL_S_DH)
      ]
  (0, DHC, Delay) ->
    mkSimpleProb
      [ (DHC, pD_Delay * pH_D_DH),
        (DLC, pD_Delay * pL_D_DH),
        (SHC, pS_Delay * pH_S_DH),
        (SLC, pS_Delay * pL_S_DH)
      ]
  (0, DLU, Start) ->
    mkSimpleProb
      [ (DHU, pD_Start * pH_D_DL * pU_D_0),
        (DHC, pD_Start * pH_D_DL * pC_D_0),
        (DLU, pD_Start * pL_D_DL * pU_D_0),
        (DLC, pD_Start * pL_D_DL * pC_D_0),
        (SHU, pS_Start * pH_S_DL * pU_S_0),
        (SHC, pS_Start * pH_S_DL * pC_S_0),
        (SLU, pS_Start * pL_S_DL * pU_S_0),
        (SLC, pS_Start * pL_S_DL * pC_S_0)
      ]
  (0, DLU, Delay) ->
    mkSimpleProb
      [ (DHU, pD_Delay * pH_D_DL * pU_D_0),
        (DHC, pD_Delay * pH_D_DL * pC_D_0),
        (DLU, pD_Delay * pL_D_DL * pU_D_0),
        (DLC, pD_Delay * pL_D_DL * pC_D_0),
        (SHU, pS_Delay * pH_S_DL * pU_S_0),
        (SHC, pS_Delay * pH_S_DL * pC_S_0),
        (SLU, pS_Delay * pL_S_DL * pU_S_0),
        (SLC, pS_Delay * pL_S_DL * pC_S_0)
      ]
  (0, DLC, Start) ->
    mkSimpleProb
      [ (DHC, pD_Start * pH_D_DL),
        (DLC, pD_Start * pL_D_DL),
        (SHC, pS_Start * pH_S_DL),
        (SLC, pS_Start * pL_S_DL)
      ]
  (0, DLC, Delay) ->
    mkSimpleProb
      [ (DHC, pD_Delay * pH_D_DL),
        (DLC, pD_Delay * pL_D_DL),
        (SHC, pS_Delay * pH_S_DL),
        (SLC, pS_Delay * pL_S_DL)
      ]
  (0, SHU, _) ->
    mkSimpleProb
      [ (SHU, pH_S_SH * pU_S_0),
        (SHC, pH_S_SH * pC_S_0),
        (SLU, pL_S_SH * pU_S_0),
        (SLC, pL_S_SH * pC_S_0)
      ]
  (0, SHC, _) ->
    mkSimpleProb
      [ (SHC, pH_S_SH),
        (SLC, pL_S_SH)
      ]
  (0, SLU, _) ->
    mkSimpleProb
      [ (SHU, pH_S_SL * pU_S_0),
        (SHC, pH_S_SL * pC_S_0),
        (SLU, pL_S_SL * pU_S_0),
        (SLC, pL_S_SL * pC_S_0)
      ]
  (0, SLC, _) ->
    mkSimpleProb
      [ (SHC, pH_S_SL),
        (SLC, pL_S_SL)
      ]
  (t, DHU, Start) ->
    mkSimpleProb
      [ (DHU, pD_Start * pH_D_DH * pU_D),
        (DHC, pD_Start * pH_D_DH * pC_D),
        (DLU, pD_Start * pL_D_DH * pU_D),
        (DLC, pD_Start * pL_D_DH * pC_D),
        (SHU, pS_Start * pH_S_DH * pU_S),
        (SHC, pS_Start * pH_S_DH * pC_S),
        (SLU, pS_Start * pL_S_DH * pU_S),
        (SLC, pS_Start * pL_S_DH * pC_S)
      ]
  (t, DHU, Delay) ->
    mkSimpleProb
      [ (DHU, pD_Delay * pH_D_DH * pU_D),
        (DHC, pD_Delay * pH_D_DH * pC_D),
        (DLU, pD_Delay * pL_D_DH * pU_D),
        (DLC, pD_Delay * pL_D_DH * pC_D),
        (SHU, pS_Delay * pH_S_DH * pU_S),
        (SHC, pS_Delay * pH_S_DH * pC_S),
        (SLU, pS_Delay * pL_S_DH * pU_S),
        (SLC, pS_Delay * pL_S_DH * pC_S)
      ]
  (t, DHC, Start) ->
    mkSimpleProb
      [ (DHC, pD_Start * pH_D_DH),
        (DLC, pD_Start * pL_D_DH),
        (SHC, pS_Start * pH_S_DH),
        (SLC, pS_Start * pL_S_DH)
      ]
  (t, DHC, Delay) ->
    mkSimpleProb
      [ (DHC, pD_Delay * pH_D_DH),
        (DLC, pD_Delay * pL_D_DH),
        (SHC, pS_Delay * pH_S_DH),
        (SLC, pS_Delay * pL_S_DH)
      ]
  (t, DLU, Start) ->
    mkSimpleProb
      [ (DHU, pD_Start * pH_D_DL * pU_D),
        (DHC, pD_Start * pH_D_DL * pC_D),
        (DLU, pD_Start * pL_D_DL * pU_D),
        (DLC, pD_Start * pL_D_DL * pC_D),
        (SHU, pS_Start * pH_S_DL * pU_S),
        (SHC, pS_Start * pH_S_DL * pC_S),
        (SLU, pS_Start * pL_S_DL * pU_S),
        (SLC, pS_Start * pL_S_DL * pC_S)
      ]
  (t, DLU, Delay) ->
    mkSimpleProb
      [ (DHU, pD_Delay * pH_D_DL * pU_D),
        (DHC, pD_Delay * pH_D_DL * pC_D),
        (DLU, pD_Delay * pL_D_DL * pU_D),
        (DLC, pD_Delay * pL_D_DL * pC_D),
        (SHU, pS_Delay * pH_S_DL * pU_S),
        (SHC, pS_Delay * pH_S_DL * pC_S),
        (SLU, pS_Delay * pL_S_DL * pU_S),
        (SLC, pS_Delay * pL_S_DL * pC_S)
      ]
  (t, DLC, Start) ->
    mkSimpleProb
      [ (DHC, pD_Start * pH_D_DL),
        (DLC, pD_Start * pL_D_DL),
        (SHC, pS_Start * pH_S_DL),
        (SLC, pS_Start * pL_S_DL)
      ]
  (t, DLC, Delay) ->
    mkSimpleProb
      [ (DHC, pD_Delay * pH_D_DL),
        (DLC, pD_Delay * pL_D_DL),
        (SHC, pS_Delay * pH_S_DL),
        (SLC, pS_Delay * pL_S_DL)
      ]
  (t, SHU, Unit) ->
    mkSimpleProb
      [ (SHU, pH_S_SH * pU_S),
        (SHC, pH_S_SH * pC_S),
        (SLU, pL_S_SH * pU_S),
        (SLC, pL_S_SH * pC_S)
      ]
  (t, SHC, Unit) ->
    mkSimpleProb
      [ (SHC, pH_S_SH),
        (SLC, pL_S_SH)
      ]
  (t, SLU, Unit) ->
    mkSimpleProb
      [ (SHU, pH_S_SL * pU_S),
        (SHC, pH_S_SL * pC_S),
        (SLU, pL_S_SL * pU_S),
        (SLC, pL_S_SL * pC_S)
      ]
  (t, SLC, Unit) ->
    mkSimpleProb
      [ (SHC, pH_S_SL),
        (SLC, pL_S_SL)
      ]
  _ -> error "Invalid state or action combination"