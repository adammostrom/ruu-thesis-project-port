import Theory
import Probabilities


-- Inititalize states
DHU :: State
DHC :: State
DLU :: State
DLC :: State
SHU :: State
SHC :: State
SLU :: State
SLC :: State

Start :: Control
Delay :: Control
Unity :: Control

isCommitted :: State -> Bool
isCommitted x = True if x == DHC ||  x == DLC  ||  x == SHC ||  x == SLC else False

isDisrupted :: State -> Bool
isDisrupted x = True if x == DLU ||  x == DLC  ||  x == SLU ||  x == SLC else False

isStarted :: State -> Bool
isStarted x = True if x == SHU ||  x == SHC  ||  x == SLU ||  x == SLC else False


availableControls _ x = Unity if isStarted == True    -- Irreversibility of choice -> sink
availableControls t x = [Start, Delay]                -- For now ignore time.

reward t x y x' = if (isCommitted x' || isDisrupted x') then 0 else 1


meas arr@(l:ls) = ...
    where z in snd unzip l

mMeas :: Time -> State -> Control -> Double
mMeas t x Unity = 0
mMeas t x _ = 1 -- Placeholder