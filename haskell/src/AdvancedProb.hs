-- |
-- Module      : AdvancedProb
-- Description : Advanced Probability Functions for SDP Calculations
-- Copyright   : (c) Group 12, 2025
-- License     : MIT
--
-- This module provides probability-related functions for Stochastic Dynamic Programming (SDP).
-- Key features:
--
-- Core Functions:
-- * Standard Normal CDF Implementation
--   - Uses Abramowitz and Stegun approximation
--   - Maximum error < 1.5e-7
--   - Efficient computation for large state spaces
--
-- Type Definitions:
-- * Val - Type alias for probability values
-- * Utility functions for probability calculations
--
-- Implementation Notes:
-- - Based on validated mathematical approximations
-- - Optimized for numerical stability
-- - Thread-safe pure functional implementation
module AdvancedProb where

-- Type alias for probabilistic values
type Val = Double

-- | Cumulative Distribution Function (CDF) for the standard normal distribution.
-- Uses Abramowitz and Stegun formula for approximation, from https://www.johndcook.com/blog/haskell-phi/.
--
-- The implementation provides a fast approximation with maximum error epsilon < 1.5e-7
--
-- @
--  cdfNorm x = P(X ≤ x) where X ~ N(0,1)
-- @
cdfNorm :: Double -> Val
cdfNorm x = y
  where
    a1 = 0.254829592
    a2 = -0.284496736
    a3 = 1.421413741
    a4 = -1.453152027
    a5 = 1.061405429
    p = 0.3275911

    -- Abramowitz and Stegun formula 7.1.26
    sign =
      if x > 0
        then 1
        else -1
    t = 1.0 / (1.0 + p * abs x / sqrt 2.0)
    e = 1.0 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * exp (-x * x / 2.0)
    y = 0.5 * (sign * e + 1.0)