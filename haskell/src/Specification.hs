module Specification where

import qualified Theory as T
import Data.List (maximumBy, nub)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

data Action = Start | Delay | Unit
  deriving (Show, Eq, Enum, Ord)


data State =  DHU | DHC | DLU | DLC | SHU | SHC | SLU | SLC
  deriving (Show, Eq, Enum, Ord)


-- Wrap the custom types with Theory's types
{- wrapState :: State -> T.State State
wrapState = T.State

wrapAction :: Action -> T.Action Action
wrapAction = T.Action -}

-- Helper: Ensure probabilities are non-negative
mkSimpleProb :: [(T.State a, Double)] -> T.Prob (T.State a)
mkSimpleProb = T.Prob . filter (\(_, p) -> p >= 0)

reward :: Int -> State -> Action -> State -> Int
reward _ _ _ next_x = if next_x == DHU || next_x == SHU then 1 else 0

-- best 
-- Compute the best action and expected value for a given time, state, and horizon
best :: Int -> Int -> State -> (Action, T.Val)
best t n x
  | n == 0 = error "Horizon must be greater than zero!"
  | otherwise =
      let ps = bi (t + 1) (n - 1)
          p = bestExt t ps
          b = fromMaybe Unit (Map.lookup x p)
          vb =  (T.val t (p : ps) x)
       in (b, vb)


-- mMeas
-- State and Action are now polymorphic in Specification
mMeas :: Int -> Int -> State -> Double
mMeas t n x
  | x `elem` [SHU, SHC, SLU, SLC] = 0
  | otherwise =
      let ps = bi t n
          ps' =
            if Map.lookup x (head ps) == Just (Start)
              then (Map.insert x Delay (head ps)) : tail ps
              else (Map.insert x Start (head ps)) : tail ps
          bestVal = val t ps x
          worstVal = val t ps' x
       in (bestVal - worstVal) / bestVal



-- BestExt  
bestExt :: Int -> T.PolicySeq (T.State a) -> T.Policy (T.State a)
bestExt t ps_tail = Map.fromList $ map bestAction states
  where
    states = [DHU, DHC, DLU, DLC]
    actions = [Start, Delay]

    -- Helper function to determine the best action for a given state
    bestAction state =
      if state `elem` states
        then
          let 
              actionValues = [(action,  (val t (Map.singleton state action : ps_tail) state)) | action <- actions]
              
              best = maximumBy (comparing snd) actionValues
           in (state, fst best)
        else (state, Start) 


-- Backwards induction - computes the optimal policy sequence
bi :: Int -> Int -> T.PolicySeq (T.State a)
bi _ 0 = []
bi t n =
  let ps_tail = bi (t + 1) (n - 1)
      p = bestExt t ps_tail  -- Assuming 'best_ext' is defined elsewhere
  in p : ps_tail


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
next :: Int -> T.State a -> T.Action a -> T.Prob (T.State a)
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