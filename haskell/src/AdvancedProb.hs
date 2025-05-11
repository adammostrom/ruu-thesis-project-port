module AdvancedProb where

import Prob
import System.Random
import Data.List (group, sort)
import Data.Function (on)

-- | Type alias for probabilistic values
type Val = Double

-- | Provide a reproducible infinite list of pseudo-random values
--   Replace seed with any fixed integer for determinism
myRandomGen :: [Double]
myRandomGen = randoms (mkStdGen 42)  -- Use a fixed seed for reproducibility

-- | Pure Box–Muller transform to convert [0,1] random values into standard normals
boxMullerTransform :: [Double] -> [Double]
boxMullerTransform (u1:u2:rest) =
  let u1' = max u1 1e-10  -- avoid log(0)
      r = sqrt (-2 * log u1')
      theta = 2 * pi * u2
      z1 = r * cos theta
      z2 = r * sin theta
  in z1 : z2 : boxMullerTransform rest
boxMullerTransform _ = []  -- In case input is too short

-- | Groups a list of continuous values into integer buckets [0..n-1]
--   and computes normalized frequencies
bucketWithProbs :: Int -> [Val] -> [(Int, Probability)]
bucketWithProbs n samples =
  let buckets = map (max 0 . min (n - 1) . floor) samples
      total = fromIntegral $ length buckets
      grouped = map (\xs -> (head xs, fromIntegral (length xs) / total))
                . group
                . sort $ buckets
  in grouped

-- | Group a list of samples into equal-sized chunks, discarding the rest
bucket :: Int -> [Val] -> [Val]
bucket n lst =
  let chunkSize = length lst `div` n
  in take chunkSize lst

-- | Pure sampling of a single normal-distributed value using Box-Muller
normal :: (Val, Val) -> StdGen -> (Val, StdGen)
normal (mu, sigma) gen0 =
  let (u1, gen1) = randomR (1e-10, 1.0) gen0
      (u2, gen2) = randomR (0.0, 1.0) gen1
      r = sqrt (-2 * log u1)
      theta = 2 * pi * u2
      z = mu + sigma * r * cos theta
  in (z, gen2)

cdfNorm :: Double -> Double
cdfNorm x = y 
    where
        a1 =  0.254829592
        a2 = -0.284496736
        a3 =  1.421413741
        a4 = -1.453152027
        a5 =  1.061405429
        p  =  0.3275911

        -- Abramowitz and Stegun formula 7.1.26
        sign = if x > 0
                   then  1
                   else -1
        t = 1.0/(1.0 + p * abs x / sqrt 2.0)
        e = 1.0 - (((((a5*t + a4)*t) + a3)*t + a2)*t + a1)*t*exp(-x*x/2.0)
        y = 0.5*(sign*e + 1.0)