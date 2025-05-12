from enum import Enum, auto
from typing import TypeAlias

from theory import SDP
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
""" pS_Start = 0.9
pD_Start = 1.0 - pS_Start

pD_Delay = 0.9
pS_Delay = 1.0 - pD_Delay """

pS_high = 0.9
pS_low = 0.1



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

    def nextFunc(self, t: int, x: State, yA: Action, yB: Action) -> dict[State, float]:
    # Define D and S states for clarity
        d_states = [State.DHU, State.DHC, State.DLU, State.DLC]
        s_states = [State.SHU, State.SHC, State.SLU, State.SLC]

        if x in s_states:
            # Transitions from S states are independent of yA and yB
            if t == 0:
                if x == State.SHU:
                    return self.mkSimpleProb([
                        (State.SHU, pH_S_SH * pU_S_0),
                        (State.SHC, pH_S_SH * pC_S_0),
                        (State.SLU, pL_S_SH * pU_S_0),
                        (State.SLC, pL_S_SH * pC_S_0),
                    ])
                elif x == State.SHC:
                    return self.mkSimpleProb([
                        (State.SHC, pH_S_SH),
                        (State.SLC, pL_S_SH),
                    ])
                elif x == State.SLU:
                    return self.mkSimpleProb([
                        (State.SHU, pH_S_SL * pU_S_0),
                        (State.SHC, pH_S_SL * pC_S_0),
                        (State.SLU, pL_S_SL * pU_S_0),
                        (State.SLC, pL_S_SL * pC_S_0),
                    ])
                elif x == State.SLC:
                    return self.mkSimpleProb([
                        (State.SHC, pH_S_SL),
                        (State.SLC, pL_S_SL),
                    ])
            elif t > 0:
                if x == State.SHU:
                    return self.mkSimpleProb([
                        (State.SHU, pH_S_SH * pU_S),
                        (State.SHC, pH_S_SH * pC_S),
                        (State.SLU, pL_S_SH * pU_S),
                        (State.SLC, pL_S_SH * pC_S),
                    ])
                elif x == State.SHC:
                    return self.mkSimpleProb([
                        (State.SHC, pH_S_SH),
                        (State.SLC, pL_S_SH),
                    ])
                elif x == State.SLU:
                    return self.mkSimpleProb([
                        (State.SHU, pH_S_SL * pU_S),
                        (State.SHC, pH_S_SL * pC_S),
                        (State.SLU, pL_S_SL * pU_S),
                        (State.SLC, pL_S_SL * pC_S),
                    ])
                elif x == State.SLC:
                    return self.mkSimpleProb([
                        (State.SHC, pH_S_SL),
                        (State.SLC, pL_S_SL),
                    ])
        elif x in d_states:
            # For D states, adjust transition probabilities based on yA and yB coherence
            if yA == Action.Start and yB == Action.Start:
                pS = pS_high  # High probability of starting when coherent
            else:
                pS = pS_low   # Low probability of starting when not coherent
            pD = 1 - pS       # Probability of staying in D states

            if t == 0:
                if x == State.DHU:
                    return self.mkSimpleProb([
                        (State.DHU, pD * pH_D_DH * pU_D_0),
                        (State.DHC, pD * pH_D_DH * pC_D_0),
                        (State.DLU, pD * pL_D_DH * pU_D_0),
                        (State.DLC, pD * pL_D_DH * pC_D_0),
                        (State.SHU, pS * pH_S_DH * pU_S_0),
                        (State.SHC, pS * pH_S_DH * pC_S_0),
                        (State.SLU, pS * pL_S_DH * pU_S_0),
                        (State.SLC, pS * pL_S_DH * pC_S_0),
                    ])
                elif x == State.DHC:
                    return self.mkSimpleProb([
                        (State.DHC, pD * pH_D_DH),
                        (State.DLC, pD * pL_D_DH),
                        (State.SHC, pS * pH_S_DH),
                        (State.SLC, pS * pL_S_DH),
                    ])
                elif x == State.DLU:
                    return self.mkSimpleProb([
                        (State.DHU, pD * pH_D_DL * pU_D_0),
                        (State.DHC, pD * pH_D_DL * pC_D_0),
                        (State.DLU, pD * pL_D_DL * pU_D_0),
                        (State.DLC, pD * pL_D_DL * pC_D_0),
                        (State.SHU, pS * pH_S_DL * pU_S_0),
                        (State.SHC, pS * pH_S_DL * pC_S_0),
                        (State.SLU, pS * pL_S_DL * pU_S_0),
                        (State.SLC, pS * pL_S_DL * pC_S_0),
                    ])
                elif x == State.DLC:
                    return self.mkSimpleProb([
                        (State.DHC, pD * pH_D_DL),
                        (State.DLC, pD * pL_D_DL),
                        (State.SHC, pS * pH_S_DL),
                        (State.SLC, pS * pL_S_DL),
                    ])
            elif t > 0:
                if x == State.DHU:
                    return self.mkSimpleProb([
                        (State.DHU, pD * pH_D_DH * pU_D),
                        (State.DHC, pD * pH_D_DH * pC_D),
                        (State.DLU, pD * pL_D_DH * pU_D),
                        (State.DLC, pD * pL_D_DH * pC_D),
                        (State.SHU, pS * pH_S_DH * pU_S),
                        (State.SHC, pS * pH_S_DH * pC_S),
                        (State.SLU, pS * pL_S_DH * pU_S),
                        (State.SLC, pS * pL_S_DH * pC_S),
                    ])
                elif x == State.DHC:
                    return self.mkSimpleProb([
                        (State.DHC, pD * pH_D_DH),
                        (State.DLC, pD * pL_D_DH),
                        (State.SHC, pS * pH_S_DH),
                        (State.SLC, pS * pL_S_DH),
                    ])
                elif x == State.DLU:
                    return self.mkSimpleProb([
                        (State.DHU, pD * pH_D_DL * pU_D),
                        (State.DHC, pD * pH_D_DL * pC_D),
                        (State.DLU, pD * pL_D_DL * pU_D),
                        (State.DLC, pD * pL_D_DL * pC_D),
                        (State.SHU, pS * pH_S_DL * pU_S),
                        (State.SHC, pS * pH_S_DL * pC_S),
                        (State.SLU, pS * pL_S_DL * pU_S),
                        (State.SLC, pS * pL_S_DL * pC_S),
                    ])
                elif x == State.DLC:
                    return self.mkSimpleProb([
                        (State.DHC, pD * pH_D_DL),
                        (State.DLC, pD * pL_D_DL),
                        (State.SHC, pS * pH_S_DL),
                        (State.SLC, pS * pL_S_DL),
                    ])
        # Default case if no match (though all states should be covered)
        return {}

    def reward(self, t: int, x: State, yA: Action, yB: Action, next_x: State) -> int:
        reward = 0
        if next_x == State.DHU or next_x == State.DHC or next_x == State.SHU or next_x == State.SHC:
            reward += 1
        if next_x == State.DLU or next_x == State.SLU or next_x == State.DHU or next_x == State.SHU:
            reward += 2
        if yA is Action.Start:
            reward -= 1
        if yB is Action.Start:
            reward -= 1
        
        return reward



SDP = MatterMost()


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