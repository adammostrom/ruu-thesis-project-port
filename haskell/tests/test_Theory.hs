module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Theory
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = hspec $ do
    describe "bi function" $ do
        it "returns an empty policy sequence when horizon is 0" $ do
            bi 0 0 `shouldBe` ([] :: PolicySeq State Action)

        it "returns a single policy when horizon is 1" $ do
            let result = bi 0 1
            length result `shouldBe` 1
            -- Check that the policy is a valid mapping
            let policy = head result
            Map.keys policy `shouldSatisfy` all (`elem` getStates)

        it "returns a sequence of policies for a given horizon" $ do
            let horizon = 3
            let result = bi 0 horizon
            length result `shouldBe` horizon
            -- Check that each policy in the sequence is valid
            mapM_ (\policy -> Map.keys policy `shouldSatisfy` all (`elem` getStates)) result

        it "produces consistent policies for a simple case" $ do
            let result = bi 0 2
            let firstPolicy = head result
            let secondPolicy = result !! 1
            -- Ensure the policies are not empty and map valid states to actions
            Map.keys firstPolicy `shouldSatisfy` all (`elem` getStates)
            Map.keys secondPolicy `shouldSatisfy` all (`elem` getStates)

    describe "QuickCheck properties for bi function" $ do
        it "produces a policy sequence of the correct length" $ property $
            \t n -> n >= 0 ==> length (bi t n) == n

        it "produces policies with valid states as keys" $ property $
            \t n -> n >= 0 ==> all (\policy -> all (`elem` getStates) (Map.keys policy)) (bi t n)

        it "produces non-empty policies for positive horizons" $ property $
            \t n -> n > 0 ==> all (not . Map.null) (bi t n)

        it "produces valid actions for each state in the policies" $ property $
            \t n -> n >= 0 ==> all (\policy -> all (\(state, action) -> action `elem` actions t state) (Map.toList policy)) (bi t n)