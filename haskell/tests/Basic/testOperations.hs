module Basic.TestOperations where

import Test.QuickCheck

-- Import the module that contains the functions to test
import Basic.Operations 

-- Example property to test
prop_additionAssociative :: Int -> Int -> Int -> Bool
prop_additionAssociative x y z = (x + y) + z == x + (y + z)

-- Main function that runs the tests
main :: IO ()
main = do
    -- Run the tests (QuickCheck will run all properties in scope)
    putStrLn "Running tests..."
    quickCheck prop_additionAssociative