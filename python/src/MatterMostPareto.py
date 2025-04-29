from enum import Enum, auto
from typing import TypeAlias

from theoryMemorization import SDP
from multiStakeholderTheory import SDP_Pareto

"""
This file contains a development of the sequential decision problem described in 
"Responsibility Under Uncertainty: Which Climate Decisions Matter Most?" by Botta et al. 
Here, the SDP has been "split" into two different SDP:s which are identical apart from 
their reward functions. Their respective results are compared to find a pareto front 
(that is, outcomes where neither one can attain a better result without the other getting 
a worse result).
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

Policy: TypeAlias = dict[State, tuple[Action, float]]
PolicySequence: TypeAlias = list[dict[State, tuple[Action, float|None]]]

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

    @property
    def zero(self) -> float:
        return 0.0
    
    @property
    def discountRate(self) -> float:
        return 1.0

    def states(self, t: int) -> list[State]:
        return list(State)

    def actions(self, t: int, x: State) -> list[Action] | list[None]:
        if x in [State.DHU, State.DHC, State.DLU, State.DLC]:
            return [Action.Start, Action.Delay]
        elif x in [State.SHU, State.SHC, State.SLU, State.SLC]:
            return [None]
        else:
            raise ValueError(f"Invalid State: '{x}'.")

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

    def reward(self, t: int, x: State, y: Action, x_prim: State) -> int:
        pass

class ClimateMatterMost(MatterMost):
    # Value is added for transitioning into states which are not comitted to
    # severe future climate change.
    def reward(self, t: int, x: State, y: Action, x_prim: State) -> float:
        return 1.0 if x_prim in [State.DHU, State.DLU, State.SHU, State.SLU] else 0.0

class EconomyMatterMost(MatterMost):
    # Value is added for transitioning into states which do not have low economic output.
    def reward(self, t: int, x: State, y: Action, x_prim: State) -> float:
        return 1.0 if x_prim in [State.DHU, State.DHC, State.SHU, State.SHC] else 0.0


SDP_Parent = MatterMost()
SDP1 = ClimateMatterMost()
SDP2 = EconomyMatterMost()

SDP_Children = [SDP1, SDP2]

Pareto = SDP_Pareto(SDP_Parent, SDP_Children)

# print(len(Pareto.children))
Pareto.valueCloud(0, 10, State.DHU, 100)
# result = Pareto.randomPS(0, 2)

# print(result)





"""
Below are functions for creating random policies / policy sequences for one SDP. Could be incorporated
into the normal theory files if wanted.

    def randomExt(self, t: int, ps_tail: PolicySequence) -> Policy:
        policy = dict()
        for state in self.states(t):
            actions = self.actions(t, state)
            random_action = random.choice(actions)
            p = {state: (random_action, None)}
            value = self.val(t, [p] + ps_tail, state)
            policy[state] = (random_action, value)
        return policy

    def randomPS(self, t: int, n: int) -> PolicySequence:
        if n == 0:
            return []
        else:
            ps_tail = self.randomPS(t + 1, n - 1)
            p = self.randomExt(t, ps_tail)
            return [p] + ps_tail
"""