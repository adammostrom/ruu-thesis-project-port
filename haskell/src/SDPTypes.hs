module SDPTypes where
import Data.Map (Map)
import Prob(Prob)
type Val = Double
type Policy x y = Map x y
type PolicySeq x y = [Policy x y]
-- SDP record
data SDP x y = SDP {
     reward   :: Int -> x -> y -> x -> Val
  ,  next     :: Int -> x -> y -> Prob x
  ,  actions  :: Int -> x -> [y]
  ,  states   :: Int -> [x]
  }
