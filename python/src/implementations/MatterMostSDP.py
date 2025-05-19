from enum import Enum, auto
from timeit import default_timer as timer

# from src.application.theory import SDP # To run SDP framework without memoization
from python.src.application.theory import SDP

"""
This is a python translation of the SDP described in the article 
"Responsibility Under Uncertainty: Which Climate Decisions Matter Most?"
by Botta et al, where memorization is used for faster computations.
"""

# Declare all states of the SDP below:
class State(Enum):
    DHU = auto()
    DHC = auto()
    DLU = auto()
    DLC = auto()
    SHU = auto()
    SHC = auto()
    SLU = auto()
    SLC = auto()

# Declare all actions of the SDP below:
class Action(Enum):
    Start = auto()
    Delay = auto()

# Define transition probabilities below:
pS_Start = 0.9
pD_Start = 1.0 - pS_Start

pD_Delay = 0.9
pS_Delay = 1.0 - pD_Delay

pL_S_DH = 0.7
pL_S_DL = 0.9
pL_S_SH = 0.3
pL_S_SL = 0.7

pH_S_DH = 1.0 - pL_S_DH
pH_S_DL = 1.0 - pL_S_DL
pH_S_SH = 1.0 - pL_S_SH
pH_S_SL = 1.0 - pL_S_SL

pL_D_DH = pL_S_SH
pL_D_DL = pL_S_SL
pH_D_DH = 1.0 - pL_D_DH
pH_D_DL = 1.0 - pL_D_DL

pU_S_0 = 0.9
pU_D_0 = 0.7
pC_S_0 = 1.0 - pU_S_0
pC_D_0 = 1.0 - pU_D_0

pU_S = 0.9
pU_D = 0.3
pC_S = 1.0 - pU_S
pC_D = 1.0 - pU_D

class MatterMost(SDP):

    # Returns the discount rate for adding rewards from later time steps 
    # (1 means no discounting takes place).
    @property
    def zero(self) -> float:
        return 0.0

    # Returns the discount rate for adding rewards from later time steps 
    # (1 means no discounting takes place).
    @property
    def discountRate(self) -> float:
        return 1.0

    # Returns all states 'x' that are valid in time step 't'.
    def states(self, t: int) -> list[State]:
        return list(State)

    # Returns all actions 'y' that are valid in time step 't' and state 'x'.
    def actions(self, t: int, x: State) -> list[Action] | list[None]:
        if x in [State.DHU, State.DHC, State.DLU, State.DLC]:
            return [Action.Start, Action.Delay]
        elif x in [State.SHU, State.SHC, State.SLU, State.SLC]:
            return [None]
        else:
            raise ValueError(f"Invalid State: '{x}'.")

    # Given a time step 't', a state 'x' and an action 'y', returns the
    # probability distribution over states to be entered in time step 't+1'.
    def nextFunc(self, t: int, x: State, y: Action) -> dict[State, float]:
        if t == 0:
            match (x, y):
                case (State.DHU, Action.Start):
                    return self.mkSimpleProb([
                        (State.DHU, pD_Start * pH_D_DH * pU_D_0),
                        (State.DHC, pD_Start * pH_D_DH * pC_D_0),
                        (State.DLU, pD_Start * pL_D_DH * pU_D_0),
                        (State.DLC, pD_Start * pL_D_DH * pC_D_0),
                        (State.SHU, pS_Start * pH_S_DH * pU_S_0),
                        (State.SHC, pS_Start * pH_S_DH * pC_S_0),
                        (State.SLU, pS_Start * pL_S_DH * pU_S_0),
                        (State.SLC, pS_Start * pL_S_DH * pC_S_0),
                    ])
                case (State.DHU, Action.Delay):
                    return self.mkSimpleProb([
                        (State.DHU, pD_Delay * pH_D_DH * pU_D_0),
                        (State.DHC, pD_Delay * pH_D_DH * pC_D_0),
                        (State.DLU, pD_Delay * pL_D_DH * pU_D_0),
                        (State.DLC, pD_Delay * pL_D_DH * pC_D_0),
                        (State.SHU, pS_Delay * pH_S_DH * pU_S_0),
                        (State.SHC, pS_Delay * pH_S_DH * pC_S_0),
                        (State.SLU, pS_Delay * pL_S_DH * pU_S_0),
                        (State.SLC, pS_Delay * pL_S_DH * pC_S_0),
                    ])
                case (State.DHC, Action.Start):
                    return self.mkSimpleProb([
                        (State.DHC, pD_Start * pH_D_DH),
                        (State.DLC, pD_Start * pL_D_DH),
                        (State.SHC, pS_Start * pH_S_DH),
                        (State.SLC, pS_Start * pL_S_DH),
                    ])
                case (State.DHC, Action.Delay):
                    return self.mkSimpleProb([
                        (State.DHC, pD_Delay * pH_D_DH),
                        (State.DLC, pD_Delay * pL_D_DH),
                        (State.SHC, pS_Delay * pH_S_DH),
                        (State.SLC, pS_Delay * pL_S_DH),
                    ])
                case (State.DLU, Action.Start):
                    return self.mkSimpleProb([
                        (State.DHU, pD_Start * pH_D_DL * pU_D_0),
                        (State.DHC, pD_Start * pH_D_DL * pC_D_0),
                        (State.DLU, pD_Start * pL_D_DL * pU_D_0),
                        (State.DLC, pD_Start * pL_D_DL * pC_D_0),
                        (State.SHU, pS_Start * pH_S_DL * pU_S_0),
                        (State.SHC, pS_Start * pH_S_DL * pC_S_0),
                        (State.SLU, pS_Start * pL_S_DL * pU_S_0),
                        (State.SLC, pS_Start * pL_S_DL * pC_S_0),
                    ])
                case (State.DLU, Action.Delay):
                    return self.mkSimpleProb([
                        (State.DHU, pD_Delay * pH_D_DL * pU_D_0),
                        (State.DHC, pD_Delay * pH_D_DL * pC_D_0),
                        (State.DLU, pD_Delay * pL_D_DL * pU_D_0),
                        (State.DLC, pD_Delay * pL_D_DL * pC_D_0),
                        (State.SHU, pS_Delay * pH_S_DL * pU_S_0),
                        (State.SHC, pS_Delay * pH_S_DL * pC_S_0),
                        (State.SLU, pS_Delay * pL_S_DL * pU_S_0),
                        (State.SLC, pS_Delay * pL_S_DL * pC_S_0),
                    ])
                case (State.DLC, Action.Start):
                    return self.mkSimpleProb([
                        (State.DHC, pD_Start * pH_D_DL),
                        (State.DLC, pD_Start * pL_D_DL),
                        (State.SHC, pS_Start * pH_S_DL),
                        (State.SLC, pS_Start * pL_S_DL),
                    ])
                case (State.DLC, Action.Delay):
                    return self.mkSimpleProb([
                        (State.DHC, pD_Delay * pH_D_DL),
                        (State.DLC, pD_Delay * pL_D_DL),
                        (State.SHC, pS_Delay * pH_S_DL),
                        (State.SLC, pS_Delay * pL_S_DL),
                    ])
                case (State.SHU, None):
                    return self.mkSimpleProb([
                        (State.SHU, pH_S_SH * pU_S_0),
                        (State.SHC, pH_S_SH * pC_S_0),
                        (State.SLU, pL_S_SH * pU_S_0),
                        (State.SLC, pL_S_SH * pC_S_0),
                    ])
                case (State.SHC, None):
                    return self.mkSimpleProb([
                        (State.SHC, pH_S_SH),
                        (State.SLC, pL_S_SH),
                    ])
                case (State.SLU, None):
                    return self.mkSimpleProb([
                        (State.SHU, pH_S_SL * pU_S_0),
                        (State.SHC, pH_S_SL * pC_S_0),
                        (State.SLU, pL_S_SL * pU_S_0),
                        (State.SLC, pL_S_SL * pC_S_0),
                    ])
                case (State.SLC, None):
                    return self.mkSimpleProb([
                        (State.SHC, pH_S_SL),
                        (State.SLC, pL_S_SL),
                    ])

        elif t > 0:
            match (x, y):
                case (State.DHU, Action.Start):
                    return self.mkSimpleProb([
                        (State.DHU, pD_Start * pH_D_DH * pU_D),
                        (State.DHC, pD_Start * pH_D_DH * pC_D),
                        (State.DLU, pD_Start * pL_D_DH * pU_D),
                        (State.DLC, pD_Start * pL_D_DH * pC_D),
                        (State.SHU, pS_Start * pH_S_DH * pU_S),
                        (State.SHC, pS_Start * pH_S_DH * pC_S),
                        (State.SLU, pS_Start * pL_S_DH * pU_S),
                        (State.SLC, pS_Start * pL_S_DH * pC_S),
                    ])
                case (State.DHU, Action.Delay):
                    return self.mkSimpleProb([
                        (State.DHU, pD_Delay * pH_D_DH * pU_D),
                        (State.DHC, pD_Delay * pH_D_DH * pC_D),
                        (State.DLU, pD_Delay * pL_D_DH * pU_D),
                        (State.DLC, pD_Delay * pL_D_DH * pC_D),
                        (State.SHU, pS_Delay * pH_S_DH * pU_S),
                        (State.SHC, pS_Delay * pH_S_DH * pC_S),
                        (State.SLU, pS_Delay * pL_S_DH * pU_S),
                        (State.SLC, pS_Delay * pL_S_DH * pC_S),
                    ])
                case (State.DHC, Action.Start):
                    return self.mkSimpleProb([
                        (State.DHC, pD_Start * pH_D_DH),
                        (State.DLC, pD_Start * pL_D_DH),
                        (State.SHC, pS_Start * pH_S_DH),
                        (State.SLC, pS_Start * pL_S_DH),
                    ])
                case (State.DHC, Action.Delay):
                    return self.mkSimpleProb([
                        (State.DHC, pD_Delay * pH_D_DH),
                        (State.DLC, pD_Delay * pL_D_DH),
                        (State.SHC, pS_Delay * pH_S_DH),
                        (State.SLC, pS_Delay * pL_S_DH),
                    ])
                case (State.DLU, Action.Start):
                    return self.mkSimpleProb([
                        (State.DHU, pD_Start * pH_D_DL * pU_D),
                        (State.DHC, pD_Start * pH_D_DL * pC_D),
                        (State.DLU, pD_Start * pL_D_DL * pU_D),
                        (State.DLC, pD_Start * pL_D_DL * pC_D),
                        (State.SHU, pS_Start * pH_S_DL * pU_S),
                        (State.SHC, pS_Start * pH_S_DL * pC_S),
                        (State.SLU, pS_Start * pL_S_DL * pU_S),
                        (State.SLC, pS_Start * pL_S_DL * pC_S),
                    ])
                case (State.DLU, Action.Delay):
                    return self.mkSimpleProb([
                        (State.DHU, pD_Delay * pH_D_DL * pU_D),
                        (State.DHC, pD_Delay * pH_D_DL * pC_D),
                        (State.DLU, pD_Delay * pL_D_DL * pU_D),
                        (State.DLC, pD_Delay * pL_D_DL * pC_D),
                        (State.SHU, pS_Delay * pH_S_DL * pU_S),
                        (State.SHC, pS_Delay * pH_S_DL * pC_S),
                        (State.SLU, pS_Delay * pL_S_DL * pU_S),
                        (State.SLC, pS_Delay * pL_S_DL * pC_S),
                    ])
                case (State.DLC, Action.Start):
                    return self.mkSimpleProb([
                        (State.DHC, pD_Start * pH_D_DL),
                        (State.DLC, pD_Start * pL_D_DL),
                        (State.SHC, pS_Start * pH_S_DL),
                        (State.SLC, pS_Start * pL_S_DL),
                    ])
                case (State.DLC, Action.Delay):
                    return self.mkSimpleProb([
                        (State.DHC, pD_Delay * pH_D_DL),
                        (State.DLC, pD_Delay * pL_D_DL),
                        (State.SHC, pS_Delay * pH_S_DL),
                        (State.SLC, pS_Delay * pL_S_DL),
                    ])
                case (State.SHU, None):
                    return self.mkSimpleProb([
                        (State.SHU, pH_S_SH * pU_S),
                        (State.SHC, pH_S_SH * pC_S),
                        (State.SLU, pL_S_SH * pU_S),
                        (State.SLC, pL_S_SH * pC_S),
                    ])
                case (State.SHC, None):
                    return self.mkSimpleProb([
                        (State.SHC, pH_S_SH),
                        (State.SLC, pL_S_SH),
                    ])
                case (State.SLU, None):
                    return self.mkSimpleProb([
                        (State.SHU, pH_S_SL * pU_S),
                        (State.SHC, pH_S_SL * pC_S),
                        (State.SLU, pL_S_SL * pU_S),
                        (State.SLC, pL_S_SL * pC_S),
                    ])
                case (State.SLC, None):
                    return self.mkSimpleProb([
                        (State.SHC, pH_S_SL),
                        (State.SLC, pL_S_SL),
                    ])

    # Given a time step 't', a state 'x' and an action 'y', returns
    # the reward of ending up in state 'x_prim' in time step 't+1'.
    def reward(self, t: int, x: State, y: Action, x_prim: State) -> int:
        return 1.0 if x_prim in [State.DHU, State.SHU] else 0.0


    
    # Function to timecheck the 
    def run_best_time(self, t, n, x):
        start = timer()
        self.best(t, n, x)
        end = timer()
        results = ((end-start))
        print(f"Time taken in ms: ")
        return(results)