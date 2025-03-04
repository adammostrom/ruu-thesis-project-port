module TotalPreorder where 


newtype Val = Val Double deriving (Show, Eq, Ord)




{-
Original Idris implementation:

> ||| TotalPreorder
> data TotalPreorder : {A : Type} -> (A -> A -> Type) -> Type where
>   MkTotalPreorder : {A : Type} ->
>                     (R : A -> A -> Type) ->
>                     (reflexive : (x : A) -> R x x) ->
>                     (transitive : (x : A) -> (y : A) -> (z : A) -> R x y -> R y z -> R x z) ->
>                     (totalPre : (x : A) -> (y : A) -> Either (R x y) (R y x)) ->
>                     TotalPreorder R


Haskell is statically typed, hence we cannot return a "Type" like in Idris.
To solve this, we use Haskells typeclass system, where we define a typeclass of TotalPreorder.

We define a data structure that encapsulates all the properties of the preorder (reflexive, transitive, and total).
-}


{-
The <= function is the binary relation between elements.
The reflexive function checks if every element is related to itself.
The transitive function ensures the transitivity condition.
The total function ensures that for any pair of elements, either x <= y or y <= x holds.

The typeclass TotalPreorder now takes a type a and defines the <=, reflexive, transitive, and totalPre methods, as in the original Idris code

-}
class TotalPreorder a where
  (<=)       :: a -> a -> Bool 
  reflexive  :: a -> Bool
  transitive :: a -> a -> a -> Bool
  totalPre   :: a -> a -> Either Bool Bool

{-
Based on the above typeclass, an instance of that typeclass can be created for the Val type.

The instance for Val implements the TotalPreorder typeclass. 
For totalPre, it uses Left to indicate x <= y and Right to indicate y <= x as in the Idris code.
-}

instance TotalPreorder Val where
    Val x <= Val y = x Prelude.<= y
    reflexive (Val x) = x Prelude.<= x
    transitive (Val x) (Val y) (Val z) = x Prelude.<= y && y Prelude.<= z
    totalPre (Val x) (Val y) = 
        if x Prelude.<= y then Left True 
        else Right True


-- The MkTotalPreorder data type encapsulates the preorder properties
data MkTotalPreorder a = MkTotalPreorder
    { mpreorderRelation :: a -> a -> Bool
    , mreflexive :: a -> Bool
    , mtransitive :: a -> a -> a -> Bool
    , mtotalPre :: a -> a -> Either Bool Bool
    }





