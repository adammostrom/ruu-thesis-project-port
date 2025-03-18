{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module CoreComputation where

import Data.List (maximumBy, nub, sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

data Action = Start | Delay | Unit
  deriving (Show, Eq, Enum, Ord, Read)

data State = DHU | DHC | DLU | DLC | SHU | SHC | SLU | SLC
  deriving (Show, Eq, Enum, Ord, Read)

type Val = Double
type Nat = Int
type Time = Int
type Horizon = Int


-- A policy maps states to actions
type Policy = Map State Action

-- A sequence of policies
type PolicySeq = [Policy]

-- Probability distribution monad
type Probability = Double
newtype Prob a = Prob {unProb :: [(a, Probability)]}
  deriving (Show, Functor)

runProb :: Eq a => Prob a -> [(a, Probability)]
runProb = collectAndSumEqual . unProb

collectAndSumEqual :: Eq a => [(a,Probability)] -> [(a, Probability)]
collectAndSumEqual aps =
  let support = nub (map fst aps)
  in map (\a -> (a, sum (map snd (filter ((a==).fst) aps))))
         support
         
-- Implementing Monad instance
instance Applicative Prob where
  pure x = Prob [(x, 1)] 
  Prob fs <*> Prob xs = Prob [(f x, p * q) | (f, p) <- fs, (x, q) <- xs]

instance Monad Prob where
  return = pure
  Prob xs >>= f = Prob [(y, p * q) | (x, p) <- xs, (y, q) <- unProb (f x)]

-- Normalize the probability distribution (so probabilities sum to 1), this is to avoid having wrongful percentage distribution
normalize :: (Ord a) => Prob a -> Prob a
normalize (Prob xs) =
  let total = sum (map snd xs)
   in Prob [(x, p / total) | (x, p) <- xs, total > 0]  -- why "total > 0"?


-- Extract the probability of a state from a Prob State
getProb :: State -> Prob State -> Maybe Probability
getProb state probState = lookup state (runProb probState)


pS_Start :: Probability
pS_Start = 0.8

pD_Start :: Probability
pD_Start = (1.0 - pS_Start)

pD_Delay :: Probability
pD_Delay = 0.9

pS_Delay :: Probability
pS_Delay = (1.0 - pD_Delay)

pL_S_DH :: Probability
pL_S_DH = 0.7

pL_S_DL :: Probability
pL_S_DL = 0.9

pL_S_SL :: Probability
pL_S_SL = 0.7

pL_S_SH :: Probability
pL_S_SH = 0.3

pL_D_DH :: Probability
pL_D_DH = pL_S_SH

pL_D_DL :: Probability
pL_D_DL = (pL_S_SL)

pU_S_0 :: Probability
pU_S_0 = 0.9

pU_D_0 :: Probability
pU_D_0 = 0.7

pU_S :: Probability
pU_S = 0.9

pU_D :: Probability
pU_D = 0.3

pH_S_DH :: Probability
pH_S_DH = (1.0 - pL_S_DH)

pH_S_SH :: Probability
pH_S_SH = (1.0 - pL_S_SH)

pH_S_DL :: Probability
pH_S_DL = (1.0 - pL_S_DL)

pH_S_SL :: Probability
pH_S_SL = (1.0 - pL_S_SL)

pH_D_DH :: Probability
pH_D_DH = (1.0 - pL_D_DH)

pH_D_DL :: Probability
pH_D_DL = (1.0 - pL_D_DL)

pC_S_0 :: Probability
pC_S_0 = (1.0 - pU_S_0)

pC_D_0 :: Probability
pC_D_0 = (1.0 - pU_D_0)

pC_S :: Probability
pC_S = (1.0 - pU_S)

pC_D :: Probability
pC_D = (1.0 - pU_D)

-- Helper: Ensure probabilities are non-negative
mkSimpleProb :: [(State, Probability)] -> Prob State
mkSimpleProb = Prob . filter (\(_, p) -> p >= 0)


{-
- Reward function

Value is added for transitioning into states which do not have low economic
output and at the same time are not comitted to severe future climate change.
-}
reward :: Int -> State -> Action -> State -> Val
reward _ _ _ next_x = if next_x == DHU || next_x == SHU then 1 else 0

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

-- PJ: It looks like you use type OtherProb a = Map a Probability instead
-- of Prob from above. A bit unclear why? (This has a different type
-- than "meas" in the Idris code. Perhaps useful, but why use the same
-- name for something different?)
-- PJ: You also have a third defintion later (called extractValue).

-- Measurement function: computes expectation
meas :: (State -> Val) -> Map State Probability -> Val
meas f dist = sum [p * f x | (x, p) <- Map.toList dist]
-- PJ: meas is never actually used, it seems

expectation :: (a -> Val) -> Prob a -> Val
expectation f (Prob xs) = sum [p * f x | (x, p) <- xs]
-- PJ: this is used a few times, but also extractValue later



-- PJ: Why return a Prob Val instead of just Val? [unexplained deviation from the Idris code]
val :: Int -> PolicySeq -> State -> Prob Val
val _ [] _ = return 0
val t (p : ps) x = do
  let y = fromMaybe Unit (Map.lookup x p)
  x' <- next t x y 
  let r = reward t x y x'   -- PJ: I simplified this line
  v <- val (t + 1) ps x'    
  return (r + v)

-- Compute the best extension of a policy sequence
best_ext :: Int -> PolicySeq -> Policy
best_ext t ps_tail = Map.fromList $ map bestAction states
  where
    states = [DHU, DHC, DLU, DLC]
    actions = [Start, Delay]

    -- Helper function to determine the best action for a given state
    bestAction state =
      if state `elem` states
        then
          let 
              actionValues = [(action, extractValue (val t (Map.singleton state action : ps_tail) state)) | action <- actions]
              
              best = maximumBy (comparing snd) actionValues
           in (state, fst best)
        else (state, Start) 

-- Extract numeric value from Prob Val (i.e., compute the expected value)
extractValue :: Prob Val -> Double
extractValue (Prob xs) = sum [p * value x | (x, p) <- xs]
  where
    value :: Val -> Double
    value = id 

-- Recursively build an optimal policy sequence
bi :: Int -> Int -> PolicySeq
bi _ 0 = []
bi t n =
  let ps_tail = bi (t + 1) (n - 1)
      p = best_ext t ps_tail
   in p : ps_tail

-- Compute the best action and expected value for a given time, state, and horizon
best :: Int -> Int -> State -> (Int, Action, Val)
best t n x
  | n == 0 = error "Horizon must be greater than zero!"
  | otherwise =
      let ps = bi (t + 1) (n - 1)
          p = best_ext t ps
          b = fromMaybe Unit (Map.lookup x p)
          vb = expectation id (val t (p : ps) x)
       in (n, b, vb)

-- Compute how much a state matters for optimal decision-making
mMeas :: Int -> Int -> State -> Double
mMeas t n x
  | x `elem` [SHU, SHC, SLU, SLC] = 0
  | otherwise =
      let ps = bi t n
          ps' =
            if Map.lookup x (head ps) == Just Start
              then (Map.insert x Delay (head ps)) : tail ps
              else (Map.insert x Start (head ps)) : tail ps
          bestVal = expectation id (val t ps x)
          worstVal = expectation id (val t ps' x)
       in (bestVal - worstVal) / bestVal

worstExt :: PolicySeq -> Policy
worstExt _ = undefined -- Placeholder

-- Extract head and tail of policy sequences
headPolicy :: PolicySeq -> Maybe Policy
headPolicy [] = Nothing
headPolicy (p:ps) = Just p

tailPolicy :: PolicySeq -> Maybe PolicySeq
tailPolicy [] = Nothing
tailPolicy (p:ps) = Just ps

-- Comparing mMeas values to those of the article, testing.
main :: IO ()
main = do
    putStrLn "Testing mMeas function..."
    print $ mMeas 0 4 DHU
    print $ mMeas 0 6 SLC
    print $ mMeas 0 7 DHU
    print $ mMeas 1 7 DHU
    print $ mMeas 3 7 DHU

-- PJ: test policy
myPol :: Policy
myPol = constPol Start

constPol :: Action -> Policy
constPol a = Map.fromList
  [ (DHU,a)
  , (DHC,a)
  , (DLU,a)
  , (DLC,a)
  , (SHU,a) 
  , (SHC,a)
  , (SLU,a)
  , (SLC,a)
  ]

testVal = val 0 [myPol] DHU 
{-
Prob {runProb =
  [ (1.0,4.899999999999998e-2)
  , (0.0,2.0999999999999998e-2)
  , (0.0,2.0999999999999994e-2)
  , (0.0,9.0e-3)
  , (1.0,0.24300000000000008)
  , (0.0,2.7e-2)
  , (0.0,0.5670000000000001)
  , (0.0,6.299999999999999e-2)
  ]}
-}

-- PaJa: some more experiments

-- Compute the optimal policy sequence starting at t=0, with n=7
optps1 :: PolicySeq
optps1 = bi 0 7
-- The distribution of values starting from DHU, following the optimal policy
mvals1 :: Prob Val
mvals1 = val 0 optps1 DHU
-- A (much) shorter representation of that same distribution.

aps1 :: [(Val, Probability)]
aps1 = runProb mvals1
-- Note that length (unProb mvals1) is 8192 but the support (length aps1) is only 8.
-- Thus the current representation is very inefficient.
-- There is no need to keep making the distribution bigger and bigger.
-- You can compute its expectation at every step and just keep the
-- result.
