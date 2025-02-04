###############################################################################
# specification.py
# A step-by-step Python translation of "Specification.lidr"
###############################################################################

# We assume you have a "theory.py" that corresponds to the Idris "Theory" module.
# For example:
#   from theory import M, val_func, ...  # or however you've structured it.
# 
# But here, we need only the M = SimpleProb reference, so let's do:

from typing import Dict, List, Callable, Tuple, Union
from enum import Enum
from theory import M  # The finite probability monad (SimpleProb in Idris)

###############################################################################
# SECTION: "We specify the stylized GHG emissions decision process"
###############################################################################
"""
[IDRIS snippet]

> Theory.M = SimpleProb
Here, 'SimpleProb' is a finite probability monad. We have that in theory.py as M.
"""

###############################################################################
# 1. STATES
###############################################################################
"""
[IDRIS snippet]

> data State = DHU | DHC | DLU | DLC | SHU | SHC | SLU | SLC
> Theory.X t = State
"""

class State(Enum):
    DHU = "DHU"
    DHC = "DHC"
    DLU = "DLU"
    DLC = "DLC"
    SHU = "SHU"
    SHC = "SHC"
    SLU = "SLU"
    SLC = "SLC"

# We'll consider `Theory.X(t)` just means `State`. 

###############################################################################
# 2. CONTROLS (StartDelay) AND Y(t,x)
###############################################################################
"""
[IDRIS snippet]

> data StartDelay = Start | Delay

-- and for states that are in the 'S'-family, there's a singleton control.
"""

class StartDelay(Enum):
    Start = "Start"
    Delay = "Delay"

# Idris code effectively does:
#   Y t DHU = StartDelay
#   Y t DHC = StartDelay
#   Y t DLU = StartDelay
#   Y t DLC = StartDelay
#   Y t SHU = Unit
#   Y t SHC = Unit
#   Y t SLU = Unit
#   Y t SLC = Unit
#
# We'll reflect that logic with a helper function:

def Y(t: int, x: State):
    """
    Returns the set of controls for state x at time t.
    In Idris, 'StartDelay' for D-states, a singleton for S-states.
    """
    if x in [State.DHU, State.DHC, State.DLU, State.DLC]:
        return [StartDelay.Start, StartDelay.Delay]
    else:
        # The Idris code uses Unit in S-states. That is a 1-element set.
        return [None]  # We'll store None for the single 'unit' control.

###############################################################################
# 3. UTILITY FUNCTIONS isCommitted, isDisrupted
###############################################################################
"""
[IDRIS snippet]

> isCommitted, isDisrupted : (t : Nat) -> X t -> Bool
> isCommitted t DHU  =  False
> isCommitted t DHC  =  True
> ...
> isDisrupted t DHU  =  False
> isDisrupted t DHC  =  False
> ...
"""

def isCommitted(x: State) -> bool:
    return x in [State.DHC, State.DLC, State.SHC, State.SLC]

def isDisrupted(x: State) -> bool:
    return x in [State.DLU, State.DLC, State.SLU, State.SLC]

###############################################################################
# 4. DEFINITION OF VARIOUS TRANSITION PROBABILITIES
###############################################################################
"""
[IDRIS snippet for pS_Start, pD_Start, etc.]

> pS_Start = cast 0.9
> pD_Start = cast 1.0 - pS_Start
> ...
> pL_S_DH = cast 0.7
> pL_S_DL = cast 0.9
> ...
> pU_S_0  = cast 0.9
> pU_D_0  = cast 0.7
> ...
> pSpec3   :  pH_S_DH `LTE` pL_S_DH
> pSpec3   =  MkLTE Oh
> ...
(and so on)
We replicate them as Python floats:
"""

pS_Start: float = 0.9
pD_Start: float = 1.0 - pS_Start

pD_Delay: float = 0.9
pS_Delay: float = 1.0 - pD_Delay

pL_S_DH: float = 0.7
pL_S_DL: float = 0.9
pL_S_SH: float = 0.3
pL_S_SL: float = 0.7

pH_S_DH: float = 1.0 - pL_S_DH
pH_S_DL: float = 1.0 - pL_S_DL
pH_S_SH: float = 1.0 - pL_S_SH
pH_S_SL: float = 1.0 - pL_S_SL

pL_D_DH: float = pL_S_SH
pL_D_DL: float = pL_S_SL
pH_D_DH: float = 1.0 - pL_D_DH
pH_D_DL: float = 1.0 - pL_D_DL

pU_S_0: float = 0.9
pU_D_0: float = 0.7
pC_S_0: float = 1.0 - pU_S_0
pC_D_0: float = 1.0 - pU_D_0

pU_S: float = 0.9
pU_D: float = 0.3
pC_S: float = 1.0 - pU_S
pC_D: float = 1.0 - pU_D

# pSpec checks:
def check_pSpec_inequalities() -> None:
    """
    Translates the Idris pSpecX lines as Python asserts.
    In Idris, these are proofs that numeric parameters satisfy certain inequalities.
    """
    # pSpec3: pH_S_DH <= pL_S_DH
    assert pH_S_DH <= pL_S_DH, "pSpec3 failed: pH_S_DH <= pL_S_DH"

    # pSpec4: pL_S_DH <= pL_S_DL
    assert pL_S_DH <= pL_S_DL, "pSpec4 failed: pL_S_DH <= pL_S_DL"

    # pSpec5: pL_S_SH <= pH_S_SH
    assert pL_S_SH <= pH_S_SH, "pSpec5 failed: pL_S_SH <= pH_S_SH"

    # pSpec6: pH_S_SL <= pL_S_SL
    assert pH_S_SL <= pL_S_SL, "pSpec6 failed: pH_S_SL <= pL_S_SL"

    # pSpec7: pL_D_DH <= pH_D_DH
    assert pL_D_DH <= pH_D_DH, "pSpec7 failed: pL_D_DH <= pH_D_DH"

    # pSpec8: pH_D_DL <= pL_D_DL
    assert pH_D_DL <= pL_D_DL, "pSpec8 failed: pH_D_DL <= pL_D_DL"

    # pSpec9: pC_S_0 <= pU_S_0
    assert pC_S_0 <= pU_S_0, "pSpec9 failed: pC_S_0 <= pU_S_0"

    # pSpec10: pC_S_0 <= pC_D_0
    assert pC_S_0 <= pC_D_0, "pSpec10 failed: pC_S_0 <= pC_D_0"

    # pSpec11: pC_S <= pU_S
    assert pC_S <= pU_S, "pSpec11 failed: pC_S <= pU_S"

    # pSpec12: pC_S <= pC_D
    assert pC_S <= pC_D, "pSpec12 failed: pC_S <= pC_D"


###############################################################################
# 5. mkSimpleProb (finite distribution builder)
###############################################################################
"""
[IDRIS snippet]

> mkSimpleProb [ (A, NonNegDouble) ... ] => SimpleProb A

We just define a Python helper that returns a dictionary from A to float.
We can rely on theory.py if you already placed it there, or define it here.
"""

def mkSimpleProb(pairs: List[Tuple[State, float]]) -> Dict[State, float]:
    dist: Dict[State, float] = {}
    for (st, pr) in pairs:
        if pr > 0:
            dist[st] = dist.get(st, 0.0) + pr
    return dist  # We do not strictly enforce sum(prob) > 0 here, but we assume it.

###############################################################################
# 6. The transition function "Theory.next"
###############################################################################
"""
[IDRIS snippet: Many lines, e.g.:

> Theory.next Z DHU Start = mkSimpleProb [
>   (DHU, pD_Start  *  pH_D_DH  *  pU_D_0), 
>   ...
> ]
> Theory.next Z DHU Delay = mkSimpleProb [
>   ...
> ]
> (etc. for DHC, DLU, DLC, SHU, SHC, SLU, SLC, plus step 1 or greater)

We replicate exactly. We'll define a Python function next_fun(t, x, y) 
that returns the distribution mkSimpleProb([...]) for each case.
We keep the product of probabilities exactly as in Idris.
"""

def next_fun(t: int, x: State, y) -> Dict[State, float]:
    # "y" can be StartDelay.Start, StartDelay.Delay, or None for S-states.
    # We do "Z" => t == 0 vs. "(S n)" => t > 0:
    
    if t == 0:
        # CASE: t == 0
        if x == State.DHU:
            if y == StartDelay.Start:
                return mkSimpleProb([
                    (State.DHU, pD_Start * pH_D_DH * pU_D_0),
                    (State.DHC, pD_Start * pH_D_DH * pC_D_0),
                    (State.DLU, pD_Start * pL_D_DH * pU_D_0),
                    (State.DLC, pD_Start * pL_D_DH * pC_D_0),
                    (State.SHU, pS_Start * pH_S_DH * pU_S_0),
                    (State.SHC, pS_Start * pH_S_DH * pC_S_0),
                    (State.SLU, pS_Start * pL_S_DH * pU_S_0),
                    (State.SLC, pS_Start * pL_S_DH * pC_S_0),
                ])
            elif y == StartDelay.Delay:
                return mkSimpleProb([
                    (State.DHU, pD_Delay * pH_D_DH * pU_D_0),
                    (State.DHC, pD_Delay * pH_D_DH * pC_D_0),
                    (State.DLU, pD_Delay * pL_D_DH * pU_D_0),
                    (State.DLC, pD_Delay * pL_D_DH * pC_D_0),
                    (State.SHU, pS_Delay * pH_S_DH * pU_S_0),
                    (State.SHC, pS_Delay * pH_S_DH * pC_S_0),
                    (State.SLU, pS_Delay * pL_S_DH * pU_S_0),
                    (State.SLC, pS_Delay * pL_S_DH * pC_S_0),
                ])
            else:
                raise ValueError("Invalid control for DHU at t=0.")
        
        elif x == State.DHC:
            if y == StartDelay.Start:
                return mkSimpleProb([
                    (State.DHC, pD_Start * pH_D_DH),
                    (State.DLC, pD_Start * pL_D_DH),
                    (State.SHC, pS_Start * pH_S_DH),
                    (State.SLC, pS_Start * pL_S_DH),
                ])
            elif y == StartDelay.Delay:
                return mkSimpleProb([
                    (State.DHC, pD_Delay * pH_D_DH),
                    (State.DLC, pD_Delay * pL_D_DH),
                    (State.SHC, pS_Delay * pH_S_DH),
                    (State.SLC, pS_Delay * pL_S_DH),
                ])
            else:
                raise ValueError("Invalid control for DHC at t=0.")
        
        elif x == State.DLU:
            if y == StartDelay.Start:
                return mkSimpleProb([
                    (State.DHU, pD_Start * pH_D_DL * pU_D_0),
                    (State.DHC, pD_Start * pH_D_DL * pC_D_0),
                    (State.DLU, pD_Start * pL_D_DL * pU_D_0),
                    (State.DLC, pD_Start * pL_D_DL * pC_D_0),
                    (State.SHU, pS_Start * pH_S_DL * pU_S_0),
                    (State.SHC, pS_Start * pH_S_DL * pC_S_0),
                    (State.SLU, pS_Start * pL_S_DL * pU_S_0),
                    (State.SLC, pS_Start * pL_S_DL * pC_S_0),
                ])
            elif y == StartDelay.Delay:
                return mkSimpleProb([
                    (State.DHU, pD_Delay * pH_D_DL * pU_D_0),
                    (State.DHC, pD_Delay * pH_D_DL * pC_D_0),
                    (State.DLU, pD_Delay * pL_D_DL * pU_D_0),
                    (State.DLC, pD_Delay * pL_D_DL * pC_D_0),
                    (State.SHU, pS_Delay * pH_S_DL * pU_S_0),
                    (State.SHC, pS_Delay * pH_S_DL * pC_S_0),
                    (State.SLU, pS_Delay * pL_S_DL * pU_S_0),
                    (State.SLC, pS_Delay * pL_S_DL * pC_S_0),
                ])
            else:
                raise ValueError("Invalid control for DLU at t=0.")
        
        elif x == State.DLC:
            if y == StartDelay.Start:
                return mkSimpleProb([
                    (State.DHC, pD_Start * pH_D_DL),
                    (State.DLC, pD_Start * pL_D_DL),
                    (State.SHC, pS_Start * pH_S_DL),
                    (State.SLC, pS_Start * pL_S_DL),
                ])
            elif y == StartDelay.Delay:
                return mkSimpleProb([
                    (State.DHC, pD_Delay * pH_D_DL),
                    (State.DLC, pD_Delay * pL_D_DL),
                    (State.SHC, pS_Delay * pH_S_DL),
                    (State.SLC, pS_Delay * pL_S_DL),
                ])
            else:
                raise ValueError("Invalid control for DLC at t=0.")
        
        elif x == State.SHU:
            # In Idris: Theory.next Z SHU () = mkSimpleProb [...]
            # The control is '()', i.e. a unit (we represent it as None in Python).
            if y is None:
                return mkSimpleProb([
                    (State.SHU, pH_S_SH * pU_S_0),
                    (State.SHC, pH_S_SH * pC_S_0),
                    (State.SLU, pL_S_SH * pU_S_0),
                    (State.SLC, pL_S_SH * pC_S_0),
                ])
            else:
                raise ValueError("Invalid control for SHU at t=0 (should be None).")
        
        elif x == State.SHC:
            if y is None:
                return mkSimpleProb([
                    (State.SHC, pH_S_SH),
                    (State.SLC, pL_S_SH),
                ])
            else:
                raise ValueError("Invalid control for SHC at t=0.")
        
        elif x == State.SLU:
            if y is None:
                return mkSimpleProb([
                    (State.SHU, pH_S_SL * pU_S_0),
                    (State.SHC, pH_S_SL * pC_S_0),
                    (State.SLU, pL_S_SL * pU_S_0),
                    (State.SLC, pL_S_SL * pC_S_0),
                ])
            else:
                raise ValueError("Invalid control for SLU at t=0.")
        
        elif x == State.SLC:
            if y is None:
                return mkSimpleProb([
                    (State.SHC, pH_S_SL),
                    (State.SLC, pL_S_SL),
                ])
            else:
                raise ValueError("Invalid control for SLC at t=0.")
        
        else:
            raise ValueError("Unexpected state at t=0.")
    
    else:
        # CASE: t > 0
        # The code is perfectly analogous but uses pU_S, pC_S, pU_D, pC_D 
        # instead of pU_S_0, pC_S_0, pU_D_0, pC_D_0.
        
        if x == State.DHU:
            if y == StartDelay.Start:
                return mkSimpleProb([
                    (State.DHU, pD_Start * pH_D_DH * pU_D),
                    (State.DHC, pD_Start * pH_D_DH * pC_D),
                    (State.DLU, pD_Start * pL_D_DH * pU_D),
                    (State.DLC, pD_Start * pL_D_DH * pC_D),
                    (State.SHU, pS_Start * pH_S_DH * pU_S),
                    (State.SHC, pS_Start * pH_S_DH * pC_S),
                    (State.SLU, pS_Start * pL_S_DH * pU_S),
                    (State.SLC, pS_Start * pL_S_DH * pC_S),
                ])
            elif y == StartDelay.Delay:
                return mkSimpleProb([
                    (State.DHU, pD_Delay * pH_D_DH * pU_D),
                    (State.DHC, pD_Delay * pH_D_DH * pC_D),
                    (State.DLU, pD_Delay * pL_D_DH * pU_D),
                    (State.DLC, pD_Delay * pL_D_DH * pC_D),
                    (State.SHU, pS_Delay * pH_S_DH * pU_S),
                    (State.SHC, pS_Delay * pH_S_DH * pC_S),
                    (State.SLU, pS_Delay * pL_S_DH * pU_S),
                    (State.SLC, pS_Delay * pL_S_DH * pC_S),
                ])
            else:
                raise ValueError("Invalid control for DHU at t>0.")
        
        elif x == State.DHC:
            if y == StartDelay.Start:
                return mkSimpleProb([
                    (State.DHC, pD_Start * pH_D_DH),
                    (State.DLC, pD_Start * pL_D_DH),
                    (State.SHC, pS_Start * pH_S_DH),
                    (State.SLC, pS_Start * pL_S_DH),
                ])
            elif y == StartDelay.Delay:
                return mkSimpleProb([
                    (State.DHC, pD_Delay * pH_D_DH),
                    (State.DLC, pD_Delay * pL_D_DH),
                    (State.SHC, pS_Delay * pH_S_DH),
                    (State.SLC, pS_Delay * pL_S_DH),
                ])
            else:
                raise ValueError("Invalid control for DHC at t>0.")
        
        elif x == State.DLU:
            if y == StartDelay.Start:
                return mkSimpleProb([
                    (State.DHU, pD_Start * pH_D_DL * pU_D),
                    (State.DHC, pD_Start * pH_D_DL * pC_D),
                    (State.DLU, pD_Start * pL_D_DL * pU_D),
                    (State.DLC, pD_Start * pL_D_DL * pC_D),
                    (State.SHU, pS_Start * pH_S_DL * pU_S),
                    (State.SHC, pS_Start * pH_S_DL * pC_S),
                    (State.SLU, pS_Start * pL_S_DL * pU_S),
                    (State.SLC, pS_Start * pL_S_DL * pC_S),
                ])
            elif y == StartDelay.Delay:
                return mkSimpleProb([
                    (State.DHU, pD_Delay * pH_D_DL * pU_D),
                    (State.DHC, pD_Delay * pH_D_DL * pC_D),
                    (State.DLU, pD_Delay * pL_D_DL * pU_D),
                    (State.DLC, pD_Delay * pL_D_DL * pC_D),
                    (State.SHU, pS_Delay * pH_S_DL * pU_S),
                    (State.SHC, pS_Delay * pH_S_DL * pC_S),
                    (State.SLU, pS_Delay * pL_S_DL * pU_S),
                    (State.SLC, pS_Delay * pL_S_DL * pC_S),
                ])
            else:
                raise ValueError("Invalid control for DLU at t>0.")
        
        elif x == State.DLC:
            if y == StartDelay.Start:
                return mkSimpleProb([
                    (State.DHC, pD_Start * pH_D_DL),
                    (State.DLC, pD_Start * pL_D_DL),
                    (State.SHC, pS_Start * pH_S_DL),
                    (State.SLC, pS_Start * pL_S_DL),
                ])
            elif y == StartDelay.Delay:
                return mkSimpleProb([
                    (State.DHC, pD_Delay * pH_D_DL),
                    (State.DLC, pD_Delay * pL_D_DL),
                    (State.SHC, pS_Delay * pH_S_DL),
                    (State.SLC, pS_Delay * pL_S_DL),
                ])
            else:
                raise ValueError("Invalid control for DLC at t>0.")
        
        elif x == State.SHU:
            if y is None:
                return mkSimpleProb([
                    (State.SHU, pH_S_SH * pU_S),
                    (State.SHC, pH_S_SH * pC_S),
                    (State.SLU, pL_S_SH * pU_S),
                    (State.SLC, pL_S_SH * pC_S),
                ])
            else:
                raise ValueError("Invalid control for SHU at t>0 (should be None).")
        
        elif x == State.SHC:
            if y is None:
                return mkSimpleProb([
                    (State.SHC, pH_S_SH),
                    (State.SLC, pL_S_SH),
                ])
            else:
                raise ValueError("Invalid control for SHC at t>0.")
        
        elif x == State.SLU:
            if y is None:
                return mkSimpleProb([
                    (State.SHU, pH_S_SL * pU_S),
                    (State.SHC, pH_S_SL * pC_S),
                    (State.SLU, pL_S_SL * pU_S),
                    (State.SLC, pL_S_SL * pC_S),
                ])
            else:
                raise ValueError("Invalid control for SLU at t>0.")
        
        elif x == State.SLC:
            if y is None:
                return mkSimpleProb([
                    (State.SHC, pH_S_SL),
                    (State.SLC, pL_S_SL),
                ])
            else:
                raise ValueError("Invalid control for SLC at t>0.")
        
        else:
            raise ValueError("Unexpected state at t>0.")

###############################################################################
# 7. Additional "pSpec" checks and "Show"/"DecEq"/"finite" logic
###############################################################################

# Show State
def show_state(st: State) -> str:
    """
    Corresponds to Idris 'implementation Show State'.
    """
    if st == State.DHU:
        return "DHU"
    if st == State.DHC:
        return "DHC"
    if st == State.DLU:
        return "DLU"
    if st == State.DLC:
        return "DLC"
    if st == State.SHU:
        return "SHU"
    if st == State.SHC:
        return "SHC"
    if st == State.SLU:
        return "SLU"
    if st == State.SLC:
        return "SLC"
    raise ValueError("Unknown State")


# Show StartDelay
def show_startdelay(sd: StartDelay) -> str:
    """
    Corresponds to Idris 'implementation Show StartDelay'.
    """
    if sd == StartDelay.Start:
        return "Start"
    else:
        return "Delay"


# A function that shows the control depending on the state
def show_control(x: State, y):
    """
    In Idris:
      show {x=DHU} y = show y
      show {x=DHC} y = show y
      ... 
      show {x=SHU} () = "()", etc.

    In Python:
      if y is None => "()"
      else         => show_startdelay(y)
    """
    if y is None:
        return "()"
    else:
        return show_startdelay(y)


# DecEq for State (in Idris we do a large pattern match)
def decEq_state(a: State, b: State) -> bool:
    """
    In Python, Enum equality is built-in. We just replicate the idea.
    """
    return a == b


# Finite control sets, and a function for a guaranteed control
def not_empty_y(x: State):
    """
    The 'Theory.notEmptyY' function in Idris returns one control
    for each state (e.g. Start or ()).
    """
    if x in [State.DHU, State.DHC, State.DLU, State.DLC]:
        return StartDelay.Start
    else:
        return None  # for S-states, equivalent to () in Idris


def finite_y(x: State):
    """
    The 'Theory.finiteY' function in Idris returns a proof that Y t x is finite.
    Here, we return a Python list of possible controls:
      - [Start, Delay] for D-states
      - [None] for S-states (Idris 'Unit').
    """
    if x in [State.DHU, State.DHC, State.DLU, State.DLC]:
        return [StartDelay.Start, StartDelay.Delay]
    else:
        return [None]
    

###############################################################################
# END OF SPECIFICATION
###############################################################################
"""
We have now mirrored the 'Specification.lidr' file into Python, 
importing 'theory.py' for the monad. All transitions are copied exactly, 
with no additional code or placeholders.

This completes our direct translation.
"""
