
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




prop_biLength :: Int -> Int -> Bool
prop_biLength t n = length (bi t n ) == n