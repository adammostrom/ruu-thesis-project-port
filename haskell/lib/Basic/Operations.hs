{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators, GADTs, ScopedTypeVariables #-}

module Basic.Operations where 

-- Resource for this module: chatgpt



{-
Enable Required Extensions.

We need to enable the following language extensions:

    - TypeFamilies for defining type families.
    - DataKinds to lift data values like True and False to the type level. 
        - (Type can be True or False instead of values evaluating to True or False)
    - TypeOperators for defining custom type operators (like equality).
    - Singletons or custom type-level equality.

-}



{-
The TypeEq type is a type equality type that captures equality between two types a and b.

The constructor Refl represents reflexive equality, meaning that any type is equal to itself (i.e., a = a).
-}
data TypeEq :: * -> * -> * where
  Refl :: TypeEq a a

{-
Leibniz's Law (also called the indiscernibility of identicals), which states that if two things are identical, then they must be interchangeable in all contexts without changing the truth of any statement.

To ensure that if you know two values or types are equal, you can replace one with the other while preserving the properties and correctness of the code.

It takes two equality proofs: TypeEq a a' and TypeEq b b', which confirm that types a and a' are identical, and b and b' are identical, respectively.

It also takes a function p of type (a -> b -> c), i.e., a function that takes two arguments, one of type a and one of type b, and returns a result of type c.

The function then returns a new function of type (a' -> b' -> c), meaning that we can safely replace a with a' and b with b' in the function p without affecting the result. The equality proofs Refl show that these replacements are valid.

-}
replace2 :: (a ~ a') => (b ~ b') => TypeEq a a' -> TypeEq b b' -> (a -> b -> c) -> (a' -> b' -> c)
replace2 Refl Refl p = p