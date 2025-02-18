module Theory  where 

--import Rel.TotalPreorder



-- NOTE: Idris code marked in comments as beginning with ">".

newtype Val = Val Double deriving (Show, Eq, Ord)
type Nat    = Int

{- > M : Type -> Type -}
type M      = Dist

-- Probability distribution type (uncertainty monad)
newtype Dist a = Dist { getDist :: [(a, Double)] } deriving (Show, Eq)

instance Functor Dist where
    fmap f (Dist xs) = Dist [(f x, p) | (x, p) <- xs]

instance Applicative Dist where
    pure x = Dist [(x, 1.0)]
    (Dist fs) <*> (Dist xs) = Dist [(f x, p1 * p2) | (f, p1) <- fs, (x, p2) <- xs]

instance Monad Dist where
    return = pure
    (Dist xs) >>= f = Dist [(y, p * p') | (x, p) <- xs, let Dist ys = f x, (y, p') <- ys]

-- Define Actions
-- Defined actions here instead of Specifications.hs
data Action = Start | Delay | Unit 
    deriving (Show, Eq, Enum)

-- States as defined from the paper, moved state from Specification.hs to Theory.hs
data State = DHU | DHC | DLU | DLC | SHU | SHC | SLU | SLC
    deriving (Show, Eq, Enum)

-- Ctrl function (Y), here a function that gives an Action
{- > X : (t : Nat) -> Type -}
class Ctrl y where
    ctrl :: y -> Action

-- "Hardcoded" control functions given a state.
{- > Y : (t : Nat) -> X t -> Type -}
instance Ctrl State where
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
next :: Nat -> State -> Action -> M State
next _ _ _ = undefined  -- Placeholder, should be implemented based on transition rules

-- Reward function
{- > reward  :  (t : Nat) -> (x : X t) -> Y t x -> X (S t) -> Val -}
reward :: Nat -> State -> Action -> State -> Val
reward _ _ _ _ = Val 0  -- Placeholder, should define actual reward logic

-- Measurement function
{- > meas :  M Val -> Val -}
meas :: Dist Val -> Val
meas (Dist xs) = Val (sum [v * p | (Val v, p) <- xs])

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

-- Value function for policy sequences
{- 
> val : Functor M => {t, n : Nat} -> PolicySeq t n -> X t -> Val
    > val {t}  Nil      x  =  zero
    > val {t} (p :: ps) x  =  let y    =  p x in
    >                         let mx'  =  next t x y in          
    >                         meas (map (reward t x y <++> val ps) mx')
-}
val :: PolicySeq -> State -> Dist Val
val Nil _ = return (Val 0)
val (Cons p ps) x = do
    let y = p x  -- Action to take based on the current state
    mx' <- next 0 x y  -- Get distribution of next states (Dist State)

    -- Map the next states to their rewards and values
    let mapped = fmap (\x' -> reward 0 x y x' `addVal` val ps x') mx'

    -- Apply meas to the mapped distribution (Dist (Dist Val)) to get the final value
    return (meas mapped)

addVal :: Val -> Dist Val -> Dist Val
addVal (Val v1) (Dist xs) = Dist [(Val (v1 + v2), p) | (Val v2, p) <- xs]

-- Placeholder for best and worst policy extensions
{-
> bestExt  :  Functor M => {t, n : Nat} -> PolicySeq (S t) n -> Policy t
-}
bestExt :: PolicySeq -> Policy
bestExt _ = ctrl  -- Placeholder

{-
> bestExtSpec  :  Functor M => {t, n : Nat} -> 
>                 (ps : PolicySeq (S t) n) -> BestExt ps (bestExt ps)
-}





worstExt :: PolicySeq -> Policy
worstExt _ = ctrl  -- Placeholder

-- Extract head and tail of policy sequences
headPolicy :: PolicySeq -> Maybe Policy
headPolicy Nil = Nothing
headPolicy (Cons p _) = Just p

tailPolicy :: PolicySeq -> Maybe PolicySeq
tailPolicy Nil = Nothing
tailPolicy (Cons _ ps) = Just ps

