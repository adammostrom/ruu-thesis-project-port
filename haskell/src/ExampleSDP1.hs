module ExampleSDP1 where

import Theory
import qualified Data.Map as Map
import Probabilities

-- SDP for a number line
data NumberLineState = Pos Int deriving (Show, Eq, Ord)
data NumberLineControl = MoveLeft | MoveRight | Stay deriving (Show, Eq, Ord)
newtype NumberLinePolicy = NumberLinePolicy (Map.Map NumberLineState NumberLineControl) deriving (Show)
data NumberLineSDP = NumberLineSDP

-- Constants for the number line
minPos :: Int
minPos = -5

maxPos :: Int
maxPos = 5

instance SDP NumberLineSDP where
  type Time NumberLineSDP = Int
  type State NumberLineSDP = NumberLineState
  type Control NumberLineSDP = NumberLineControl
  type Policy NumberLineSDP = NumberLinePolicy
  type Val NumberLineSDP = Double

  -- Available controls depend on position
  availableControls _ (Pos x)
    | x == minPos = [MoveRight, Stay]
    | x == maxPos = [MoveLeft, Stay]
    | otherwise = [MoveLeft, MoveRight, Stay]

  -- All possible states
  states _ = [Pos x | x <- [minPos .. maxPos]]

  -- Reward: negative distance from origin
  reward _ (Pos x) _ _ = -fromIntegral (abs x)

  -- Deterministic transition
  next _ (Pos x) MoveLeft = [(Pos (x - 1), 1.0)]
  next _ (Pos x) MoveRight = [(Pos (x + 1), 1.0)]
  next _ (Pos x) Stay = [(Pos x, 1.0)]

  -- Apply policy
  applyPolicy (NumberLinePolicy p) s = Map.findWithDefault Stay s p

  -- Expected value
  meas rpPairs = sum [xi * pi | (xi, pi) <- rpPairs]

-- Example usage
main :: IO ()
main = do
  putStrLn "Starting at position 3, time 0, horizon 3:"
  putStrLn $ best (0 :: Int) 3 (Pos 3)