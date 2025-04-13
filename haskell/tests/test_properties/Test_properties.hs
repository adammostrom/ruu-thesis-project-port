module Test_properties where


import Theory hiding (State, Action)
import Test.QuickCheck

data State = S1 | S2 | S3 | S4 | S5 deriving (Show, Eq, Enum, Ord, Read)
data Action = A1 | A2 | A3 deriving (Show, Eq, Enum, Ord, Read)

instance Theory State Action where
    
    -- actions :: Int -> State -> [Action]
    actions _ s 
        | s `elem` [S1, S2, S3] = [A1, A2]
        | s `elem` [S4, S5]     = [A3]
        | otherwise             = error "no legitimate states"
    
    
    states _ = [S1, S2, S3, S4, S5]



{-
Testing of function: BI
-}
prop_biLength :: Int -> Int -> Bool
prop_biLength t n = length (bi t n ) == n

{- prop_biIsOptimal :: Positive Int -> State -> Bool
prop_biIsOptimal (Positive n) s = 
    let opt   = val 0 (bi 0 n) s
        worst = (buildWorstSeq 0 n)
    in opt >= bad

buildWorstSeq :: Int -> Int -> PolicySeq State Action
buildWorstSeq -}