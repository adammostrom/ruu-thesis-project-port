{-# LANGUAGE DeriveFunctor #-}
module Prob where
import Data.List (maximumBy, minimumBy, nub)
type Probability = Double
-- Probability distribution monad, (State, Probability)
newtype Prob a = Prob {unProb :: [(a, Probability)]}
  deriving (Show, Functor)

-- helper function to extract all the probabilities from the uncertainty monad
weights :: Prob a -> [Probability]
weights (Prob xs) = map snd xs

-- No negative probabilities.
validProb :: Prob a -> Bool
validProb (Prob xs) =
  let ws = weights (Prob xs) in all (>0) ws && sum ws > 0

normalize :: Prob a -> Prob a
normalize (Prob xs) =
  let total = sum (map snd xs )
  in if total == 0 then error "Total probability equals to zero, cannot normalize"
     else Prob [(x, p / total) | (x,p) <- xs]

prob :: (Eq a) => Prob a -> a -> Probability
prob (Prob xs) x =
  let total = sum (map snd xs)
      matching = sum [p | (x', p) <- xs, x == x']
  in if total == 0 then 0 else matching / total

-- Implementing Monad instance
instance Applicative Prob where
  pure x = Prob [(x, 1)]
  Prob fs <*> Prob xs = Prob [(f x, p * q) | (f, p) <- fs, (x, q) <- xs]

instance Monad Prob where
  return = pure
  Prob xs >>= f = normalize . Prob $ [(y, p * q) | (x, p) <- xs, let Prob ys = normalize (f x), (y, q) <- ys]

runProb :: (Eq a) => Prob a -> [(a, Probability)]
runProb = collectAndSumEqual . unProb

collectAndSumEqual :: (Eq a) => [(a, Probability)] -> [(a, Probability)]
collectAndSumEqual aps =
  let support = nub (map fst aps)
   in map
        (\a -> (a, sum (map snd (filter ((a ==) . fst) aps))))
        support

-- Helper: Ensure probabilities are non-negative
mkSimpleProb :: [(x, Probability)] -> Prob x
mkSimpleProb = Prob . filter (\(_, p) -> p >= 0)

expectation :: Eq a => (a -> Double) -> Prob a -> Double
expectation f aps = sum (map (\(a, pr) -> pr * f a) (runProb aps))
