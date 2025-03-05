module Theory where

-- import Rel.TotalPreorder

-- NOTE: Idris code marked in comments as beginning with ">".

import Control.Monad (liftM2)


-- Probability distribution type (uncertainty monad)
newtype Dist a = Dist {getDist :: [(a, Double)]} deriving (Show, Eq)

newtype Val = Val Double deriving (Show, Eq, Ord)

type Nat = Int
type Time = Int
type Horizon = Int

-- Policy Type
{-
> Policy : (t : Nat) -> Type
> Policy t = (x : X t) -> Y t x
-}
type Policy = State -> Action

{-
>data PolicySeq : (t : Nat) -> (n : Nat) -> Type where
    >   Nil   :  {t : Nat} -> PolicySeq t Z
    >   (::)  :  {t, n : Nat} -> Policy t -> PolicySeq (S t) n -> PolicySeq t (S n)
-}
data PolicySeq = Nil | Cons Policy PolicySeq


instance Functor Dist where
  fmap f (Dist xs) = Dist [(f x, p) | (x, p) <- xs]

instance Applicative Dist where
  pure x = Dist [(x, 1.0)]
  Dist fs <*> Dist xs = Dist [(f x, p1 * p2) | (f, p1) <- fs, (x, p2) <- xs]

instance Monad Dist where
  return = pure
  Dist xs >>= f = Dist [(y, p * p') | (x, p) <- xs, let Dist ys = f x, (y, p') <- ys]

-- Define Actions
-- Defined actions here instead of Specifications.hs
data Action = Start | Delay | Unit
  deriving (Show, Eq, Enum)

-- States as defined from the paper, moved state from Specification.hs to Theory.hs
data State = DHU | DHC | DLU | DLC | SHU | SHC | SLU | SLC
  deriving (Show, Eq, Enum)

-- Example of a Policy
ctrl :: Policy -- State -> Action
ctrl DHU = Start
ctrl DHC = Delay
ctrl DLU = Unit
ctrl DLC = Unit
ctrl SHU = Start
ctrl SHC = Delay
ctrl SLU = Unit
ctrl SLC = Unit

-- Successor function
newtype S t = S t

-- Next state transition
{-  > next : (t : Nat) -> (x : X t) -> Y t x -> M (X (S t)) -}
-- Transition function (we will mock the transition probabilities based on actions)
nextT :: State -> Action -> Dist State
nextT DHU Start = Dist [(DHC, 0.9), (DLU, 0.1)] -- Example, can modify based on your actual logic

    -- This is a strange example: deciding to start the transition
    -- should lead with at least a non-zero probability to some state
    -- with "S" as the first letter.

nextT DHC Delay = Dist [(DHU, 0.8), (DHC, 0.2)]
nextT _ _ = Dist [(DHU, 1.0)] -- Default transition

-- Reward function
{- > reward  :  (t : Nat) -> (x : X t) -> Y t x -> X (S t) -> Val -}
reward :: State -> Action -> State -> Val
reward SHU _ _ = Val 1
reward _ _ _ = Val 0

-- Measurement function
{- > meas :  M Val -> Val -}
-- Measurement function (combines values with their probabilities)
meas :: Dist Val -> Val
meas (Dist xs) = Val (sum [v * p | (Val v, p) <- xs])



-- Value function for policy sequences
{-
> val : Functor M => {t, n : Nat} -> PolicySeq t n -> X t -> Val
    > val {t}  Nil      x  =  zero
    > val {t} (p :: ps) x  =  let y    =  p x in
    >                         let mx'  =  next t x y in
    >                         meas (map (reward t x y <++> val ps) mx')
-}
-- Value function (recursive calculation)
val :: PolicySeq -> State -> Dist Val   -- PJ: why Dist Val and not just Val?
val Nil _ = return (Val 0)
val (Cons p ps)  x = do
    let y = p x
    mx' <- nextT x y
    Dist [(Val v, p) | (Val v, p) <- getDist (fmap (\x' -> addVal (reward x y x') (val ps x')) mx')]
      -- strange deviation from the Idris code (perhaps due to wrong type?)

addVal :: Val -> Dist Val -> Dist Val
addVal (Val v1) (Dist xs) = Dist [(Val (v1 + v2), p) | (Val v2, p) <- xs]
-- Why do you want to define addition for this assymetric type?

-- Placeholder for best and worst policy extensions
{-
> bestExt  :  Functor M => {t, n : Nat} -> PolicySeq (S t) n -> Policy t
-}
-- Function to compute the best action given a policy sequence
bestExt :: PolicySeq -> Policy
bestExt _ = ctrl -- Placeholder, for now returns the ctrl function for each state

{-
> bestExtSpec  :  Functor M => {t, n : Nat} ->
>                 (ps : PolicySeq (S t) n) -> BestExt ps (bestExt ps)
-}

worstExt :: PolicySeq -> Policy
worstExt _ = ctrl -- Placeholder

-- Extract head and tail of policy sequences
headPolicy :: PolicySeq -> Maybe Policy
headPolicy Nil = Nothing
headPolicy (Cons p _) = Just p

tailPolicy :: PolicySeq -> Maybe PolicySeq
tailPolicy Nil = Nothing
tailPolicy (Cons _ ps) = Just ps
