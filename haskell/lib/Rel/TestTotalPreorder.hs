module TestTotalPreorder where

import Test.QuickCheck
import TotalPreorder


{-
QuickCheck needs to generate random values of type Val.

The Arbitrary typeclass is used by QuickCheck to generate random values for testing. 

This will automatically allow QuickCheck to generate random Val values for testing. The arbitrary function here uses the arbitrary function from the Arbitrary instance of Double to create random Double values, and then wraps them in the Val constructor.
-}
instance Arbitrary Val where
    arbitrary = Val <$> arbitrary


-- Reflexive property: For any Val x, x <= x
prop_reflexive :: Val -> Bool
prop_reflexive x = reflexive x == (x Prelude.<= x)

-- Transitive property: If x <= y and y <= z, then x <= z
prop_transitive ::  Val -> Val -> Val -> Bool
prop_transitive x y z = 
    if (x Prelude.<= y && y Prelude.<= z) then (x Prelude.<= z) else True

-- TotalPreorder property: For any x and y, either x <= y or y <= x
prop_totalPre :: Val -> Val -> Bool
prop_totalPre x y = case totalPre x y of
    Left _  -> x Prelude.<= y
    Right _ -> y Prelude.<= x


-- Example TotalPreorder instance for Val
exampleTotalPreorder :: MkTotalPreorder Val
exampleTotalPreorder = MkTotalPreorder
    { mpreorderRelation = (TotalPreorder.<=)
    , mreflexive = reflexive
    , mtransitive = transitive
    , mtotalPre = totalPre
    }

mainEx :: IO ()
mainEx = do
    let val1 = Val 3.0
        val2 = Val 5.0
    print (mpreorderRelation exampleTotalPreorder val1 val2)  -- Should print True
    print (mreflexive exampleTotalPreorder val1)             -- Should print True
    print (mtransitive exampleTotalPreorder val1 val2 (Val 8.0)) -- Should print True
    print (mtotalPre exampleTotalPreorder val1 val2)        -- Should print Left True
    

mainQc :: IO ()
mainQc = do
    quickCheck prop_reflexive
    quickCheck prop_transitive
    quickCheck prop_totalPre


