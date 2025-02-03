module Appendix where

import Data.Void (Void)

type A = ()

type B = ()

type C = ()

type FALSE = Void

-- Head is a standard haskell function
headL :: [a] -> a
headL (x : xs) = head xs

{-

Idris uses type-checker which check the type of the function and the type of the arguments.
Haskell uses type-inference which infers the type of the function and the type of the arguments.

Dependently typed langauges support types which depend on values.

The source code specify computations using properties-as-types
and apply the Idris type checker to verify that the computations satisfy
their specifications.

-}

