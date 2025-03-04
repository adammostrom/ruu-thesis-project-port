{-# LANGUAGE DeriveGeneric #-}

module Specification where

import GHC.Generics (Generic)
import Theory (Dist(..), Action (..), Nat, State (..), next)

-- import Data.Map (Map)
-- import qualified Data.Map as Map

-- Double distribution type
type ProbDist a = [(a, Double)]



{- > isCommitted  :  (t : Nat) -> X t -> Bool -}
isCommited :: Nat -> State -> Bool
isCommited = undefined

{- > isDisrupted  :  (t : Nat) -> X t -> Bool -}
isDisrupted :: Nat -> State -> Bool
isDisrupted = undefined

{-
> pS_Start : NonNegDouble
> pS_Start = cast 0.9
-}
pS_Start :: Double
pS_Start =  0.9

{-
> pD_Start : NonNegDouble
> pD_Start = cast 1.0 - pS_Start
-}
pD_Start :: Double
pD_Start =  (1.0 - pS_Start)

{-
> pD_Delay  :  NonNegDouble
> pD_Delay  =  cast 0.9
-}
pD_Delay :: Double
pD_Delay =  0.9

{-
> pS_Delay  :  NonNegDouble
> pS_Delay  =  cast 1.0 - pD_Delay
-}
pS_Delay :: Double
pS_Delay =  (1.0 - pD_Delay)

{-
> pL_S_DH  :  NonNegDouble
> pL_S_DL  :  NonNegDouble
> pL_S_SH  :  NonNegDouble
> pL_S_SL  :  NonNegDouble
> pL_D_DH  :  NonNegDouble
> pL_D_DL  :  NonNegDouble
-}

{-
"Because pH_S_DH = 1 - pL_S_DH, this requires pL_S_DH to be greater
or equal to 50%. Let's say that
    pL_S_DH = cast 0.7"
-}
pL_S_DH :: Double
pL_S_DH =  0.7

{-
We also want to express the idea that starting a green transition in a
weak economy (perhaps a suboptimal decision?) is more likely to yield a
weak economy than starting a green transition in a strong economy

which requires specifying a value of pL_S_DL between 0.7 and 1.0, say

> pL_S_DL = cast 0.9

-}
pL_S_DL :: Double
pL_S_DL =  0.9

{-
In this situation, and again because of the inertia of economic systems,
it is reasonable to assume that transitions from H-states (booming
economy) to H-states are more likely than transitions from H-states to
L-states and, of course, the other way round. In formulas:

Conversely, a low value of pL_S_SH means high resilience
against economic downturns in states in which a transition towards a
globally decarbonized society has been started or has been
accomplished.

In such states, we assume a moderate likelihood of fast recovering from
economic downturns:

> pL_S_SL  =  cast 0.7

and also a moderate resilience

> pL_S_SH  =  cast 0.3

-}
pL_S_SL :: Double
pL_S_SL =  0.7

pL_S_SH :: Double
pL_S_SH =  0.3

{-
Let's turn the attention to the last two transition probabilities that
need to be specified in order to complete the description of the
transitions leading to economic downturns or recoveries. These are
pL_D_DH and pL_D_DL.

The semantics of pL_D_DH and pL_D_DL should meanwhile be clear:
pL_D_DH represents the Double of economic downturns and 1 -
pL_D_DL the Double of recovering (from economic downturns) in
states in which a green transition has not already been started. As for
their counterparts discussed above, we have the semantic requirements

Realistic answers to this question are likely to depend on the decision
step and on the time elapsed since the green transition has been
started, see \ref{subsection:realistic}. As a first approximation, here
we just assume that these probabilities are the same:

> pL_D_DL  =  pL_S_SL
>
> pL_D_DH  =  pL_S_SH

-}
pL_D_DH :: Double
pL_D_DH =  pL_S_SH

pL_D_DL :: Double
pL_D_DL =  (pL_S_SL)

{-
> pU_S_0  :  NonNegDouble
> pU_S_0  =  cast 0.9

> pU_D_0  :  NonNegDouble
> pU_D_0  =  cast 0.7

> pU_S    :  NonNegDouble
> pU_S    =  cast 0.9

> pU_D    :  NonNegDouble
> pU_D    =  cast 0.3
-}

pU_S_0 :: Double
pU_S_0 =  0.9

pU_D_0 :: Double
pU_D_0 =  0.7

pU_S :: Double
pU_S =  0.9

pU_D :: Double
pU_D =  0.3

mkSimpleProb :: Dist State -> Dist State
mkSimpleProb = id -- Placeholder for now, could normalize probs if needed

{-
NOTE: Was outcommented in the original code. Not sure if they should be included:

> pH_S_DH  :  NonNegDouble
> pH_S_DH  =  cast 1.0 - pL_S_DH

> pH_S_SH  :  NonNegDouble
> pH_S_SH  =  cast 1.0 - pL_S_SH

> pH_S_DL  :  NonNegDouble
> pH_S_DL  =  cast 1.0 - pL_S_DL

> pH_S_SL  :  NonNegDouble
> pH_S_SL  =  cast 1.0 - pL_S_SL

> pH_D_DH  :  NonNegDouble
> pH_D_DH  =  cast 1.0 - pL_D_DH

> pH_D_DL  :  NonNegDouble
> pH_D_DL  =  cast 1.0 - pL_D_DL

> pC_S_0   :  NonNegDouble
> pC_S_0   =  cast 1.0 - pU_S_0

> pC_D_0   :  NonNegDouble
> pC_D_0   =  cast 1.0 - pU_D_0

> pC_S     :  NonNegDouble
> pC_S     =  cast 1.0 - pU_S

> pC_D     :  NonNegDouble
> pC_D     =  cast 1.0 - pU_D

-}

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

{- The next Function implemented in Theory.hs -}
next :: State -> Action -> Dist State
next DHU Start =
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
next DHU Delay =
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
next DHC Start = 
    mkSimpleProb
    [ (DHC, pD_Start * pH_D_DH),
      (DLC, pD_Start * pL_D_DH),
      (SHC, pS_Start * pH_S_DH),
      (SLC, pS_Start * pL_S_DH)
    ]
next DHC Delay = 
    mkSimpleProb
    [   (DHC, pD_Delay * pH_D_DH),
        (DLC, pD_Delay * pL_D_DH),
        (SHC, pS_Delay * pH_S_DH),
        (SLC, pS_Delay * pL_S_SH)
    ]
next  DLU Start = 
  mkSimpleProb
[  (DHU,  pD_Start  *  pH_D_DL  *  pU_D_0),
   (DHC,  pD_Start  *  pH_D_DL  *  pC_D_0),
   (DLU,  pD_Start  *  pL_D_DL  *  pU_D_0),
   (DLC,  pD_Start  *  pL_D_DL  *  pC_D_0),
   (SHU,  pS_Start  *  pH_S_DL  *  pU_S_0),
   (SHC,  pS_Start  *  pH_S_DL  *  pC_S_0),
   (SLU,  pS_Start  *  pL_S_DL  *  pU_S_0),
   (SLC,  pS_Start  *  pL_S_DL  *  pC_S_0)]
next  DLU Delay = 
  mkSimpleProb
[  (DHU,  pD_Delay  *  pH_D_DL  *  pU_D_0),
   (DHC,  pD_Delay  *  pH_D_DL  *  pC_D_0),
   (DLU,  pD_Delay  *  pL_D_DL  *  pU_D_0),
   (DLC,  pD_Delay  *  pL_D_DL  *  pC_D_0),
   (SHU,  pS_Delay  *  pH_S_DL  *  pU_S_0),
   (SHC,  pS_Delay  *  pH_S_DL  *  pC_S_0),
   (SLU,  pS_Delay  *  pL_S_DL  *  pU_S_0),
   (SLC,  pS_Delay  *  pL_S_DL  *  pC_S_0)]
next  DLC Start = 
  mkSimpleProb
[  (DHC,  pD_Start  *  pH_D_DL),
   (DLC,  pD_Start  *  pL_D_DL),
   (SHC,  pS_Start  *  pH_S_DL),
   (SLC,  pS_Start  *  pL_S_DL)]
next  DLC Delay = 
  mkSimpleProb
[  (DHC,  pD_Delay  *  pH_D_DL),
   (DLC,  pD_Delay  *  pL_D_DL),
   (SHC,  pS_Delay  *  pH_S_DL),
   (SLC,  pS_Delay  *  pL_S_DL)]

next  SHU () = 
  mkSimpleProb
      [  (SHU,  pH_S_SH  *  pU_S_0),
         (SHC,  pH_S_SH  *  pC_S_0),
         (SLU,  pL_S_SH  *  pU_S_0),
         (SLC,  pL_S_SH  *  pC_S_0)]
   
next  SHC () = 
  mkSimpleProb
       [  (SHC,  pH_S_SH),
          (SLC,  pL_S_SH)]
next  SLU () = 
  mkSimpleProb
       [  (SHU,  pH_S_SL  *  pU_S_0),
          (SHC,  pH_S_SL  *  pC_S_0),
          (SLU,  pL_S_SL  *  pU_S_0),
          (SLC,  pL_S_SL  *  pC_S_0)]
   
next  SLC () = 
  mkSimpleProb
       [  (SHC,  pH_S_SL),
          (SLC,  pL_S_SL)]

next (S n) DHU Start = mkSimpleProb
       [  (DHU,  pD_Start  *  pH_D_DH  *  pU_D),
          (DHC,  pD_Start  *  pH_D_DH  *  pC_D),
          (DLU,  pD_Start  *  pL_D_DH  *  pU_D),
          (DLC,  pD_Start  *  pL_D_DH  *  pC_D),
          (SHU,  pS_Start  *  pH_S_DH  *  pU_S),
          (SHC,  pS_Start  *  pH_S_DH  *  pC_S),
          (SLU,  pS_Start  *  pL_S_DH  *  pU_S),
          (SLC,  pS_Start  *  pL_S_DH  *  pC_S)]
   
next (S n) DHU Delay = mkSimpleProb
       [  (DHU,  pD_Delay  *  pH_D_DH  *  pU_D),
          (DHC,  pD_Delay  *  pH_D_DH  *  pC_D),
          (DLU,  pD_Delay  *  pL_D_DH  *  pU_D),
          (DLC,  pD_Delay  *  pL_D_DH  *  pC_D),
          (SHU,  pS_Delay  *  pH_S_DH  *  pU_S),
          (SHC,  pS_Delay  *  pH_S_DH  *  pC_S),
          (SLU,  pS_Delay  *  pL_S_DH  *  pU_S),
          (SLC,  pS_Delay  *  pL_S_DH  *  pC_S)]
   
next (S n) DHC Start = mkSimpleProb
       [  (DHC,  pD_Start  *  pH_D_DH),
          (DLC,  pD_Start  *  pL_D_DH),
          (SHC,  pS_Start  *  pH_S_DH),
          (SLC,  pS_Start  *  pL_S_DH)]
   
next (S n) DHC Delay = mkSimpleProb
       [  (DHC,  pD_Delay  *  pH_D_DH),
          (DLC,  pD_Delay  *  pL_D_DH),
          (SHC,  pS_Delay  *  pH_S_DH),
          (SLC,  pS_Delay  *  pL_S_DH)]
   
next (S n) DLU Start = mkSimpleProb
       [  (DHU,  pD_Start  *  pH_D_DL  *  pU_D),
          (DHC,  pD_Start  *  pH_D_DL  *  pC_D),
          (DLU,  pD_Start  *  pL_D_DL  *  pU_D),
          (DLC,  pD_Start  *  pL_D_DL  *  pC_D),
          (SHU,  pS_Start  *  pH_S_DL  *  pU_S),
          (SHC,  pS_Start  *  pH_S_DL  *  pC_S),
          (SLU,  pS_Start  *  pL_S_DL  *  pU_S),
          (SLC,  pS_Start  *  pL_S_DL  *  pC_S)]
   
next (S n) DLU Delay = mkSimpleProb
       [  (DHU,  pD_Delay  *  pH_D_DL  *  pU_D),
          (DHC,  pD_Delay  *  pH_D_DL  *  pC_D),
          (DLU,  pD_Delay  *  pL_D_DL  *  pU_D),
          (DLC,  pD_Delay  *  pL_D_DL  *  pC_D),
          (SHU,  pS_Delay  *  pH_S_DL  *  pU_S),
          (SHC,  pS_Delay  *  pH_S_DL  *  pC_S),
          (SLU,  pS_Delay  *  pL_S_DL  *  pU_S),
          (SLC,  pS_Delay  *  pL_S_DL  *  pC_S)]
   
next (S n) DLC Start = mkSimpleProb
       [  (DHC,  pD_Start  *  pH_D_DL),
          (DLC,  pD_Start  *  pL_D_DL),
          (SHC,  pS_Start  *  pH_S_DL),
          (SLC,  pS_Start  *  pL_S_DL)]
   
next (S n) DLC Delay = mkSimpleProb
       [  (DHC,  pD_Delay  *  pH_D_DL),
          (DLC,  pD_Delay  *  pL_D_DL),
          (SHC,  pS_Delay  *  pH_S_DL),
          (SLC,  pS_Delay  *  pL_S_DL)]
   
next (S n) SHU () = mkSimpleProb
       [  (SHU,  pH_S_SH  *  pU_S),
          (SHC,  pH_S_SH  *  pC_S),
          (SLU,  pL_S_SH  *  pU_S),
          (SLC,  pL_S_SH  *  pC_S)]
   
next (S n) SHC () = mkSimpleProb
       [  (SHC,  pH_S_SH),
          (SLC,  pL_S_SH)]
   
next (S n) SLU () = mkSimpleProb
       [  (SHU,  pH_S_SL  *  pU_S),
          (SHC,  pH_S_SL  *  pC_S),
          (SLU,  pL_S_SL  *  pU_S),
         (SLC,  pL_S_SL  *  pC_S)]
   
next (S n) SLC () = mkSimpleProb
       [  (SHC,  pH_S_SL),
          (SLC,  pL_S_SL)]



implementation Show State where
show DHU = "DHU"
show DHC = "DHC"
show DLU = "DLU"
show DLC = "DLC"
show SHU = "SHU"
show SHC = "SHC"
show SLU = "SLU"
show SLC = "SLC"

showX : {t : Nat} -> X t -> String
showX x = show x

implementation Show StartDelay where
   show Start = "Start"
   show Delay = "Delay"

show : {t : Nat} -> {x : X t} -> Y t x -> String
show {x = DHU} y = show y
show {x = DHC} y = show y
show {x = DLU} y = show y
show {x = DLC} y = show y
show {x = SHU} y = show y
show {x = SHC} y = show y
show {x = SLU} y = show y
show {x = SLC} y = show y

to : StartDelay -> Fin 2
to Start  =     FZ
to Delay  =  FS FZ

from : Fin 2 -> StartDelay
from             FZ   = Start
from         (FS FZ)  = Delay
from     (FS (FS  k)) = absurd k

toFrom : (k : Fin 2) -> to (from k) = k
toFrom             FZ   = Refl
toFrom         (FS FZ)  = Refl
toFrom     (FS (FS  k)) = absurd k

fromTo : (a : StartDelay) -> from (to a) = a
fromTo Start  =  Refl
fromTo Delay  =  Refl

finiteStartDelay : Finite StartDelay
finiteStartDelay = MkSigma 2 (MkIso to from toFrom fromTo)


isCommitted t DHU  =  False
isCommitted t DHC  =  True
isCommitted t DLU  =  False
isCommitted t DLC  =  True
isCommitted t SHU  =  False
isCommitted t SHC  =  True
isCommitted t SLU  =  False
isCommitted t SLC  =  True

isDisrupted t DHU  =  False
isDisrupted t DHC  =  False
isDisrupted t DLU  =  True
isDisrupted t DLC  =  True
isDisrupted t SHU  =  False
isDisrupted t SHC  =  False
isDisrupted t SLU  =  True
isDisrupted t SLC  =  True






{-
%if False

> Theory.Y t DHU = StartDelay
> Theory.Y t DHC = StartDelay
> Theory.Y t DLU = StartDelay
> Theory.Y t DLC = StartDelay

> Theory.Y t SHU = Unit
> Theory.Y t SHC = Unit
> Theory.Y t SLU = Unit
> Theory.Y t SLC = Unit


> implementation DecEq State where
>   decEq DHU DHU = Yes Refl
>   decEq DHU DHC = let contra = \ Refl impossible in No contra
>   decEq DHU DLU = let contra = \ Refl impossible in No contra
>   decEq DHU DLC = let contra = \ Refl impossible in No contra
>   decEq DHU SHU = let contra = \ Refl impossible in No contra
>   decEq DHU SHC = let contra = \ Refl impossible in No contra
>   decEq DHU SLU = let contra = \ Refl impossible in No contra
>   decEq DHU SLC = let contra = \ Refl impossible in No contra
>
>   decEq DHC DHU = let contra = \ Refl impossible in No contra
>   decEq DHC DHC = Yes Refl
>   decEq DHC DLU = let contra = \ Refl impossible in No contra
>   decEq DHC DLC = let contra = \ Refl impossible in No contra
>   decEq DHC SHU = let contra = \ Refl impossible in No contra
>   decEq DHC SHC = let contra = \ Refl impossible in No contra
>   decEq DHC SLU = let contra = \ Refl impossible in No contra
>   decEq DHC SLC = let contra = \ Refl impossible in No contra
>
>   decEq DLU DHU = let contra = \ Refl impossible in No contra
>   decEq DLU DHC = let contra = \ Refl impossible in No contra
>   decEq DLU DLU = Yes Refl
>   decEq DLU DLC = let contra = \ Refl impossible in No contra
>   decEq DLU SHU = let contra = \ Refl impossible in No contra
>   decEq DLU SHC = let contra = \ Refl impossible in No contra
>   decEq DLU SLU = let contra = \ Refl impossible in No contra
>   decEq DLU SLC = let contra = \ Refl impossible in No contra
>
>   decEq DLC DHU = let contra = \ Refl impossible in No contra
>   decEq DLC DHC = let contra = \ Refl impossible in No contra
>   decEq DLC DLU = let contra = \ Refl impossible in No contra
>   decEq DLC DLC = Yes Refl
>   decEq DLC SHU = let contra = \ Refl impossible in No contra
>   decEq DLC SHC = let contra = \ Refl impossible in No contra
>   decEq DLC SLU = let contra = \ Refl impossible in No contra
>   decEq DLC SLC = let contra = \ Refl impossible in No contra
>
>   decEq SHU DHU = let contra = \ Refl impossible in No contra
>   decEq SHU DHC = let contra = \ Refl impossible in No contra
>   decEq SHU DLU = let contra = \ Refl impossible in No contra
>   decEq SHU DLC = let contra = \ Refl impossible in No contra
>   decEq SHU SHU = Yes Refl
>   decEq SHU SHC = let contra = \ Refl impossible in No contra
>   decEq SHU SLU = let contra = \ Refl impossible in No contra
>   decEq SHU SLC = let contra = \ Refl impossible in No contra
>
>   decEq SHC DHU = let contra = \ Refl impossible in No contra
>   decEq SHC DHC = let contra = \ Refl impossible in No contra
>   decEq SHC DLU = let contra = \ Refl impossible in No contra
>   decEq SHC DLC = let contra = \ Refl impossible in No contra
>   decEq SHC SHU = let contra = \ Refl impossible in No contra
>   decEq SHC SHC = Yes Refl
>   decEq SHC SLU = let contra = \ Refl impossible in No contra
>   decEq SHC SLC = let contra = \ Refl impossible in No contra
>
>   decEq SLU DHU = let contra = \ Refl impossible in No contra
>   decEq SLU DHC = let contra = \ Refl impossible in No contra
>   decEq SLU DLU = let contra = \ Refl impossible in No contra
>   decEq SLU DLC = let contra = \ Refl impossible in No contra
>   decEq SLU SHU = let contra = \ Refl impossible in No contra
>   decEq SLU SHC = let contra = \ Refl impossible in No contra
>   decEq SLU SLU = Yes Refl
>   decEq SLU SLC = let contra = \ Refl impossible in No contra
>
>   decEq SLC DHU = let contra = \ Refl impossible in No contra
>   decEq SLC DHC = let contra = \ Refl impossible in No contra
>   decEq SLC DLU = let contra = \ Refl impossible in No contra
>   decEq SLC DLC = let contra = \ Refl impossible in No contra
>   decEq SLC SHU = let contra = \ Refl impossible in No contra
>   decEq SLC SHC = let contra = \ Refl impossible in No contra
>   decEq SLC SLU = let contra = \ Refl impossible in No contra
>   decEq SLC SLC = Yes Refl


> Theory.notEmptyY t DHU = Start
> Theory.notEmptyY t DHC = Start
> Theory.notEmptyY t DLU = Start
> Theory.notEmptyY t DLC = Start
> Theory.notEmptyY t SHU = ()
> Theory.notEmptyY t SHC = ()
> Theory.notEmptyY t SLU = ()
> Theory.notEmptyY t SLC = ()

> Theory.finiteY t DHU = finiteStartDelay
> Theory.finiteY t DHC = finiteStartDelay
> Theory.finiteY t DLU = finiteStartDelay
> Theory.finiteY t DLC = finiteStartDelay
> Theory.finiteY t SHU = finiteUnit
> Theory.finiteY t SHC = finiteUnit
> Theory.finiteY t SLU = finiteUnit
> Theory.finiteY t SLC = finiteUnit
-}