module Basic.Predicates where

import Data.Void (Void)

{-
A function from a to void.

Idris equivalent:
   Empty : Type -> Type
   Empty A = A -> Void
-}
type Empty a = a -> Void

{-
Iso type.

Isomorphic type describes a one-to-one mapping between objects in other fields
Basically, it means to be identical or similar in form, shape or structure.

Hence a and b are isomorphic because we can go from a to b and go from b to a.
-}

data Iso a b = Iso {to :: a -> b, from :: b -> a}

{-
Singleton type.

Here we use the Iso type and we are saying that a is isomorphic to ().
Basically, a is similar to the "empty tuple", that is has exactly one value.
-}
type Singleton a = Iso a ()


----------------- TODO : LATER ON DEMAND 

{-

{-# LANGUAGE DataKinds #-}

-- Define a data type that can hold either `True` or `False` at the type level
data Truth (t :: Bool) = T | F

{-
Unlike Idris, Haskell does not support dependent types natively (types depending on values), so we can't fully express dependent types in the same way as Idris does.
-}
{-# LANGUAGE TypeFamilies #-}

type Nat = Integer

-- Define a type family for IsSingleton
type family IsSingleton (x :: Bool) :: * where
  IsSingleton (Truth T)  = Nat
  IsSingleton Truth 'False  = [Nat]

mkSingle :: Bool -> IsSingleton x -> IsSingleton x
mkSingle True _ = 0
mkSingle False _ = []

-}