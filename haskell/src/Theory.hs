module Theory where 

--import Rel.TotalPreorder

newtype Val = Val Double deriving (Show, Eq, Ord)
type Nat    = Int
type M      = Dist

-- Define the successor function S t as t + 1
type S t = (t + 1)

-- Probability distribution type (uncertainty monad)
newtype Dist a = Dist [(a, Double)]  -- A list of possible values and their associated probabilities

-- Functor instance for Dist
instance Functor Dist where
  fmap f (Dist xs) = Dist [(f x, p) | (x, p) <- xs]

-- Monad instance for Dist
instance Monad Dist where
  return x = Dist [(x, 1.0)]  -- A distribution with one possible outcome and probability 1
  (Dist xs) >>= f = Dist [ (y, p * p') | (x, p) <- xs, (Dist ys) <- [f x], (y, p') <- ys ]




data Action = Start | Delay | Unit 
    deriving (Show, Eq, Enum)

class Ctrl y t where
    ctrl :: State y Nat t-> Action

instance Ctrl State where
    ctrl DHU = Start
    ctrl DHC = Delay
    ctrl DLU = Unit
    ctrl DLC = Unit
    ctrl SHU = Start
    ctrl SHC = Delay
    ctrl SLU = Unit
    ctrl SLC = Unit




data State = DHU | DHC | DLU | DLC | SHU | SHC | SLU | SLC
  deriving (Show, Eq, Enum)


-- next : (t : Nat) -> (x : X t) -> Y t x -> M (X (S t))

next :: Nat -> State t -> Ctrl t (State t) -> M (State (S t))
next = undefined


-- Val       :  Type
-- reward    :  (t : Nat) -> (x : X t) -> Y t x -> X (S t) -> Val

reward :: Nat -> State t -> Ctrl t (State t) -> State (S t) -> Val
reward = undefined
-- (<+>)     :  Val -> Val -> Val

-- meas             :  M Val -> Val

meas :: M Val -> Val
meas = undefined 



-- Policy : (t : Nat) -> Type
-- Policy t = (x : X t) -> Y t x

{- Represented as a function type. -}
type Policy t = State t -> Ctrl t (State t)


-- data PolicySeq : (t : Nat) -> (n : Nat) -> Type where
--   Nil   :  {t : Nat} -> PolicySeq t Z
--   (::)  :  {t, n : Nat} -> Policy t -> PolicySeq (S t) n -> PolicySeq t (S n)

{- TODO: Implement the successor "S" (type level increment). "Peano arithmetic" -}
data PolicySeq t n = Nil | Cons (Policy t) (PolicySeq (S t) n)



-- val : Functor M => {t, n : Nat} -> PolicySeq t n -> X t -> Val
-- val {t}  Nil      x  =  zero
-- val {t} (p :: ps) x  =  let y    =  p x in
--                         let mx'  =  next t x y in          
--                         meas (map (reward t x y <++> val ps) mx')

{- Functor m represents the M monad. -}
val :: Functor m => PolicySeq t n -> State t -> m Val
val Nil x = return zero
val (Cons p ps) x = do
    let y = p x
    mx' <- next t x y
    meas $ fmap (\x' -> reward t x y x' + val ps x') mx'




{-

> OptPolicySeq  :  Functor M => {t, n : Nat} -> PolicySeq t n -> Type
> OptPolicySeq {t} {n} ps  =  (ps' : PolicySeq t n) -> (x : X t) -> val ps' x <= val ps x

> BestExt  :  Functor M => {t, n : Nat} -> PolicySeq (S t) n -> Policy t -> Type
> BestExt {t} ps p  =  (p' : Policy t) -> (x : X t) -> val (p' :: ps) x <= val (p :: ps) x


> Bellman  :  Functor M => {t, n : Nat} ->
>             (ps   :  PolicySeq (S t) n) -> OptPolicySeq ps ->
>             (p    :  Policy t)          -> BestExt ps p ->
>             OptPolicySeq (p :: ps)


> plusMon   :  {v1, v2, v3, v4 : Val} -> 
>              v1 <= v2 -> v3 <= v4 -> (v1 <+> v3) <= (v2 <+> v4)
>
> measMon   :  Functor M => {A : Type} -> 
>              (f, g : A -> Val) -> ((a : A) -> f a <= g a) ->
>              (ma : M A) -> meas (map f ma) <= meas (map g ma)


> bestExt      :  Functor M => {t, n : Nat} -> PolicySeq (S t) n -> Policy t

> bestExtSpec  :  Functor M => {t, n : Nat} -> 
>                 (ps : PolicySeq (S t) n) -> BestExt ps (bestExt ps)


> bi  :   Functor M => (t : Nat) -> (n : Nat) -> PolicySeq t n
> bi t  Z     =  Nil
> bi t (S n)  =  let ps = bi (S t) n in bestExt ps :: ps

> nilOptPolicySeq  :  Functor M => OptPolicySeq Nil
> nilOptPolicySeq Nil x = reflexive lteTP zero


> biLemma  :  Functor M => (t : Nat) -> (n : Nat) -> OptPolicySeq (bi t n)


> biLemma t  Z     =  nilOptPolicySeq
> biLemma t (S n)  =  Bellman ps ops p oep
>   where  ps   :  PolicySeq (S t) n
>          ps   =  bi (S t) n
>          ops  :  OptPolicySeq ps
>          ops  =  biLemma (S t) n
>          p    :  Policy t
>          p    =  bestExt ps
>          oep  :  BestExt ps p
>          oep  =  bestExtSpec ps

> notEmptyY : (t : Nat) -> (x : X t) -> Y t x


> cval  :  Functor M => {t, n : Nat} -> 
>          PolicySeq (S t) n -> (x : X t) -> Y t x -> Val
> cval {t} ps x y  =  let mx' = next t x y in
>                     meas (map (reward t x y <++> val ps) mx')



> cvalmax     :  Functor M => {t, n : Nat} -> 
>                PolicySeq (S t) n -> (x : X t) -> Val

> cvalargmax  :  Functor M => {t, n : Nat} -> 
>                PolicySeq (S t) n -> (x : X t) -> Y t x


> cvalmaxSpec  :  Functor M => {t, n : Nat} -> 
>                 (ps : PolicySeq (S t) n) -> (x : X t) -> 
>                 (y : Y t x) -> cval ps x y <= cvalmax ps x

> cvalargmaxSpec  :  Functor M => {t, n : Nat} -> 
>                    (ps : PolicySeq (S t) n) -> (x  : X t) ->
>                    cvalmax ps x = cval ps x (cvalargmax ps x)



> bestExt = cvalargmax

, one has

> bestExtSpec {t} {n} ps p' x = s4 where
>   p     :  Policy t
>   p     =  bestExt ps
>   y     :  Y t x
>   y     =  p x
>   y'    :  Y t x
>   y'    =  p' x
>   s1    :  cval ps x y' <= cvalmax ps x
>   s1    =  cvalmaxSpec ps x y'
>   s2    :  cval ps x y' <= cval ps x (cvalargmax ps x)
>   s2    =  replace {P = \ z => (cval ps x y' <= z)} (cvalargmaxSpec ps x) s1
>   s3    :  cval ps x y' <= cval ps x y
>   s3    =  s2
>   s4    :  val (p' :: ps) x <= val (p :: ps) x
>   s4    =  s3



* Extensions (worst extension):

> worstExt  :  Functor M => {t, n : Nat} -> PolicySeq (S t) n -> Policy t

> cvalmin     :  Functor M => {t, n : Nat} -> 
>                PolicySeq (S t) n -> (x : X t) -> Val

> cvalargmin  :  Functor M => {t, n : Nat} -> 
>                PolicySeq (S t) n -> (x : X t) -> Y t x

> cvalminSpec  :  Functor M => {t, n : Nat} -> 
>                 (ps : PolicySeq (S t) n) -> (x : X t) -> 
>                 (y : Y t x) -> cvalmin ps x <= cval ps x y

> cvalargminSpec  :  Functor M => {t, n : Nat} -> 
>                    (ps : PolicySeq (S t) n) -> (x  : X t) ->
>                    cvalmin ps x = cval ps x (cvalargmin ps x)

> worstExt = cvalargmin

> cvalmin {t} ps x = Opt.Operations.min lteTP (finiteY t x) (cardNotZero t x) (cval ps x)

> cvalargmin {t} ps x = Opt.Operations.argmin lteTP (finiteY t x) (cardNotZero t x) (cval ps x)

> cvalminSpec {t} ps x = Opt.Operations.minSpec lteTP (finiteY t x) (cardNotZero t x) (cval ps x)

> cvalargminSpec {t} ps x = Opt.Operations.argminSpec lteTP (finiteY t x) (cardNotZero t x) (cval ps x)


> head  :  {t, n : Nat} -> PolicySeq t (S n) -> Policy t
> head (p :: ps) = p

> tail  :  {t, n : Nat} -> PolicySeq t (S n) -> PolicySeq (S t) n
> tail (p :: ps) = ps 



> data StateCtrlSeq  :  (t, n : Nat) -> Type where
>   Last  :  {t : Nat} -> X t -> StateCtrlSeq t (S Z)
>   (##)  :  {t, n : Nat} -> DPair (X t) (Y t) -> StateCtrlSeq (S t) (S n) -> StateCtrlSeq t (S (S n))


> trj  :  Monad M => {t, n : Nat} -> PolicySeq t n -> X t -> M (StateCtrlSeq t (S n))
> trj {t}  Nil      x  =  pure (Last x)
> trj {t} (p :: ps) x  =  let y   = p x in
>                         let mx' = next t x y in
>                         map ((MkDPair x y) ##) (mx' >>= trj ps)

-}
