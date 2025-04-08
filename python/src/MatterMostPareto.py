from enum import Enum, auto

import numpy as np
from theoryMemorization import SDP

import random
import matplotlib.pyplot as plt

"""
This file contains a modification of the SDP described in "Responsibility Under Uncertainty: 
Which Climate Decisions Matter Most?" by Botta et al. Here, the SDP has been "split" into
two different SDP:s which are identical apart from their reward functions. Their respective results
are compared to find a pareto front (that is, outcomes where neither one can attain a better result
without the other getting a worse result).
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
    @property
    def states(self) -> list[State]:
        return list(State)

    # Function that returns the possible actions in any allowed state.
    def actions(self, t: int, x: Enum) -> list[str] | list[None]:
        if x in [State.DHU, State.DHC, State.DLU, State.DLC]:
            return [Action.Start, Action.Delay]
        elif x in [State.SHU, State.SHC, State.SLU, State.SLC]:
            return [None]
        else:
            raise ValueError(f"Invalid State: '{x}'.")


    # nextFunc takes in timestep t, state x, and action (control) y.
    def nextFunc(self, t: int, x: State, y: State) -> dict[State, float]:
        # "y" can be Action.Start, Action.Delay, or None for S-states.
        if t < 0 or type(t) != int:
            raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")
        if t == 0:
            # CASE: t == 0
            if x == State.DHU:
                if y == Action.Start:
                    return self.mkSimpleProb(
                        [
                            (State.DHU, pD_Start * pH_D_DH * pU_D_0),
                            (State.DHC, pD_Start * pH_D_DH * pC_D_0),
                            (State.DLU, pD_Start * pL_D_DH * pU_D_0),
                            (State.DLC, pD_Start * pL_D_DH * pC_D_0),
                            (State.SHU, pS_Start * pH_S_DH * pU_S_0),
                            (State.SHC, pS_Start * pH_S_DH * pC_S_0),
                            (State.SLU, pS_Start * pL_S_DH * pU_S_0),
                            (State.SLC, pS_Start * pL_S_DH * pC_S_0),
                        ]
                    )
                elif y == Action.Delay:
                    return self.mkSimpleProb(
                        [
                            (State.DHU, pD_Delay * pH_D_DH * pU_D_0),
                            (State.DHC, pD_Delay * pH_D_DH * pC_D_0),
                            (State.DLU, pD_Delay * pL_D_DH * pU_D_0),
                            (State.DLC, pD_Delay * pL_D_DH * pC_D_0),
                            (State.SHU, pS_Delay * pH_S_DH * pU_S_0),
                            (State.SHC, pS_Delay * pH_S_DH * pC_S_0),
                            (State.SLU, pS_Delay * pL_S_DH * pU_S_0),
                            (State.SLC, pS_Delay * pL_S_DH * pC_S_0),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for DHU at t=0.")

            elif x == State.DHC:
                if y == Action.Start:
                    return self.mkSimpleProb(
                        [
                            (State.DHC, pD_Start * pH_D_DH),
                            (State.DLC, pD_Start * pL_D_DH),
                            (State.SHC, pS_Start * pH_S_DH),
                            (State.SLC, pS_Start * pL_S_DH),
                        ]
                    )
                elif y == Action.Delay:
                    return self.mkSimpleProb(
                        [
                            (State.DHC, pD_Delay * pH_D_DH),
                            (State.DLC, pD_Delay * pL_D_DH),
                            (State.SHC, pS_Delay * pH_S_DH),
                            (State.SLC, pS_Delay * pL_S_DH),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for DHC at t=0.")

            elif x == State.DLU:
                if y == Action.Start:
                    return self.mkSimpleProb(
                        [
                            (State.DHU, pD_Start * pH_D_DL * pU_D_0),
                            (State.DHC, pD_Start * pH_D_DL * pC_D_0),
                            (State.DLU, pD_Start * pL_D_DL * pU_D_0),
                            (State.DLC, pD_Start * pL_D_DL * pC_D_0),
                            (State.SHU, pS_Start * pH_S_DL * pU_S_0),
                            (State.SHC, pS_Start * pH_S_DL * pC_S_0),
                            (State.SLU, pS_Start * pL_S_DL * pU_S_0),
                            (State.SLC, pS_Start * pL_S_DL * pC_S_0),
                        ]
                    )
                elif y == Action.Delay:
                    return self.mkSimpleProb(
                        [
                            (State.DHU, pD_Delay * pH_D_DL * pU_D_0),
                            (State.DHC, pD_Delay * pH_D_DL * pC_D_0),
                            (State.DLU, pD_Delay * pL_D_DL * pU_D_0),
                            (State.DLC, pD_Delay * pL_D_DL * pC_D_0),
                            (State.SHU, pS_Delay * pH_S_DL * pU_S_0),
                            (State.SHC, pS_Delay * pH_S_DL * pC_S_0),
                            (State.SLU, pS_Delay * pL_S_DL * pU_S_0),
                            (State.SLC, pS_Delay * pL_S_DL * pC_S_0),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for DLU at t=0.")

            elif x == State.DLC:
                if y == Action.Start:
                    return self.mkSimpleProb(
                        [
                            (State.DHC, pD_Start * pH_D_DL),
                            (State.DLC, pD_Start * pL_D_DL),
                            (State.SHC, pS_Start * pH_S_DL),
                            (State.SLC, pS_Start * pL_S_DL),
                        ]
                    )
                elif y == Action.Delay:
                    return self.mkSimpleProb(
                        [
                            (State.DHC, pD_Delay * pH_D_DL),
                            (State.DLC, pD_Delay * pL_D_DL),
                            (State.SHC, pS_Delay * pH_S_DL),
                            (State.SLC, pS_Delay * pL_S_DL),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for DLC at t=0.")

            elif x == State.SHU:
                # In Idris: Theory.next Z SHU () = mkSimpleProb [...]
                # The control is '()', i.e. a unit (we represent it as None in Python).
                if y is None:
                    return self.mkSimpleProb(
                        [
                            (State.SHU, pH_S_SH * pU_S_0),
                            (State.SHC, pH_S_SH * pC_S_0),
                            (State.SLU, pL_S_SH * pU_S_0),
                            (State.SLC, pL_S_SH * pC_S_0),
                        ]
                    )
                else:
                    raise ValueError(f"Invalid control for SHU at t=0 (should be None), actual: {y}" )

            elif x == State.SHC:
                if y is None:
                    return self.mkSimpleProb(
                        [
                            (State.SHC, pH_S_SH),
                            (State.SLC, pL_S_SH),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for SHC at t=0.")

            elif x == State.SLU:
                if y is None:
                    return self.mkSimpleProb(
                        [
                            (State.SHU, pH_S_SL * pU_S_0),
                            (State.SHC, pH_S_SL * pC_S_0),
                            (State.SLU, pL_S_SL * pU_S_0),
                            (State.SLC, pL_S_SL * pC_S_0),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for SLU at t=0.")

            elif x == State.SLC:
                if y is None:
                    return self.mkSimpleProb(
                        [
                            (State.SHC, pH_S_SL),
                            (State.SLC, pL_S_SL),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for SLC at t=0.")

            else:
                raise ValueError("Unexpected state at t=0.")

        else:
            # CASE: t > 0
            # The code is perfectly analogous but uses pU_S, pC_S, pU_D, pC_D
            # instead of pU_S_0, pC_S_0, pU_D_0, pC_D_0.

            if x == State.DHU:
                if y == Action.Start:
                    return self.mkSimpleProb(
                        [
                            (State.DHU, pD_Start * pH_D_DH * pU_D),
                            (State.DHC, pD_Start * pH_D_DH * pC_D),
                            (State.DLU, pD_Start * pL_D_DH * pU_D),
                            (State.DLC, pD_Start * pL_D_DH * pC_D),
                            (State.SHU, pS_Start * pH_S_DH * pU_S),
                            (State.SHC, pS_Start * pH_S_DH * pC_S),
                            (State.SLU, pS_Start * pL_S_DH * pU_S),
                            (State.SLC, pS_Start * pL_S_DH * pC_S),
                        ]
                    )
                elif y == Action.Delay:
                    return self.mkSimpleProb(
                        [
                            (State.DHU, pD_Delay * pH_D_DH * pU_D),
                            (State.DHC, pD_Delay * pH_D_DH * pC_D),
                            (State.DLU, pD_Delay * pL_D_DH * pU_D),
                            (State.DLC, pD_Delay * pL_D_DH * pC_D),
                            (State.SHU, pS_Delay * pH_S_DH * pU_S),
                            (State.SHC, pS_Delay * pH_S_DH * pC_S),
                            (State.SLU, pS_Delay * pL_S_DH * pU_S),
                            (State.SLC, pS_Delay * pL_S_DH * pC_S),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for DHU at t>0.")

            elif x == State.DHC:
                if y == Action.Start:
                    return self.mkSimpleProb(
                        [
                            (State.DHC, pD_Start * pH_D_DH),
                            (State.DLC, pD_Start * pL_D_DH),
                            (State.SHC, pS_Start * pH_S_DH),
                            (State.SLC, pS_Start * pL_S_DH),
                        ]
                    )
                elif y == Action.Delay:
                    return self.mkSimpleProb(
                        [
                            (State.DHC, pD_Delay * pH_D_DH),
                            (State.DLC, pD_Delay * pL_D_DH),
                            (State.SHC, pS_Delay * pH_S_DH),
                            (State.SLC, pS_Delay * pL_S_DH),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for DHC at t>0.")

            elif x == State.DLU:
                if y == Action.Start:
                    return self.mkSimpleProb(
                        [
                            (State.DHU, pD_Start * pH_D_DL * pU_D),
                            (State.DHC, pD_Start * pH_D_DL * pC_D),
                            (State.DLU, pD_Start * pL_D_DL * pU_D),
                            (State.DLC, pD_Start * pL_D_DL * pC_D),
                            (State.SHU, pS_Start * pH_S_DL * pU_S),
                            (State.SHC, pS_Start * pH_S_DL * pC_S),
                            (State.SLU, pS_Start * pL_S_DL * pU_S),
                            (State.SLC, pS_Start * pL_S_DL * pC_S),
                        ]
                    )
                elif y == Action.Delay:
                    return self.mkSimpleProb(
                        [
                            (State.DHU, pD_Delay * pH_D_DL * pU_D),
                            (State.DHC, pD_Delay * pH_D_DL * pC_D),
                            (State.DLU, pD_Delay * pL_D_DL * pU_D),
                            (State.DLC, pD_Delay * pL_D_DL * pC_D),
                            (State.SHU, pS_Delay * pH_S_DL * pU_S),
                            (State.SHC, pS_Delay * pH_S_DL * pC_S),
                            (State.SLU, pS_Delay * pL_S_DL * pU_S),
                            (State.SLC, pS_Delay * pL_S_DL * pC_S),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for DLU at t>0.")

            elif x == State.DLC:
                if y == Action.Start:
                    return self.mkSimpleProb(
                        [
                            (State.DHC, pD_Start * pH_D_DL),
                            (State.DLC, pD_Start * pL_D_DL),
                            (State.SHC, pS_Start * pH_S_DL),
                            (State.SLC, pS_Start * pL_S_DL),
                        ]
                    )
                elif y == Action.Delay:
                    return self.mkSimpleProb(
                        [
                            (State.DHC, pD_Delay * pH_D_DL),
                            (State.DLC, pD_Delay * pL_D_DL),
                            (State.SHC, pS_Delay * pH_S_DL),
                            (State.SLC, pS_Delay * pL_S_DL),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for DLC at t>0.")

            elif x == State.SHU:
                if y is None:
                    return self.mkSimpleProb(
                        [
                            (State.SHU, pH_S_SH * pU_S),
                            (State.SHC, pH_S_SH * pC_S),
                            (State.SLU, pL_S_SH * pU_S),
                            (State.SLC, pL_S_SH * pC_S),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for SHU at t>0 (should be None).")

            elif x == State.SHC:
                if y is None:
                    return self.mkSimpleProb(
                        [
                            (State.SHC, pH_S_SH),
                            (State.SLC, pL_S_SH),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for SHC at t>0.")

            elif x == State.SLU:
                if y is None:
                    return self.mkSimpleProb(
                        [
                            (State.SHU, pH_S_SL * pU_S),
                            (State.SHC, pH_S_SL * pC_S),
                            (State.SLU, pL_S_SL * pU_S),
                            (State.SLC, pL_S_SL * pC_S),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for SLU at t>0.")

            elif x == State.SLC:
                if y is None:
                    return self.mkSimpleProb(
                        [
                            (State.SHC, pH_S_SL),
                            (State.SLC, pL_S_SL),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for SLC at t>0.")

            else:
                raise ValueError("Unexpected state at t>0.")

            # Testing the transition function.

    def reward(self, t: int, x: State, y: Action, next_x: State) -> int:
        pass
    
    def randomExt(self, t: int, ps_tail: list[dict[State, tuple[Action, float]]]) -> dict[State, tuple[Action, float]]:
        policy = dict()
        for state in self.states:
            actions = self.actions(t, state)
            random_action = random.choice(actions)
            p = {state: (random_action, None)}
            value = self.val(t, [p] + ps_tail, state)
            policy[state] = (random_action, value)
        return policy

    def randomPS(self, t: int, n: int) -> list[dict[State, tuple[Action, float]]]:
        if n == 0:
            return []
        else:
            ps_tail = self.randomPS(t + 1, n - 1)
            p = self.randomExt(t, ps_tail)
            return [p] + ps_tail

class ClimateMatterMost(MatterMost):
    def reward(self, t: int, x: State, y: Action, next_x: State) -> int:
        # Value is added for transitioning into states which are not comitted to
        # severe future climate change.
        return 1.0 if next_x in [State.DHU, State.DLU, State.SHU, State.SLU] else 0.0

class EconomyMatterMost(MatterMost):
    def reward(self, t: int, x: State, y: Action, next_x: State) -> int:
        # Value is added for transitioning into states which do not have
        # low economic output.
        return 1.0 if next_x in [State.DHU, State.DHC, State.SHU, State.SHC] else 0.0


SDP_parent = MatterMost()
SDP1 = ClimateMatterMost()
SDP2 = EconomyMatterMost()


def randomExtGlobal(SDP_parent, SDP1, SDP2, t: int,
                    ps_tail_1: list[dict[State, tuple[Action, float]]],
                    ps_tail_2: list[dict[State, tuple[Action, float]]]) -> dict[State, tuple[Action, float]]:
    policy_SDP1 = dict() # These two will be equal policies
    policy_SDP2 = dict() # but with different values.
    for state in SDP_parent.states:
        actions = SDP_parent.actions(t, state)
        random_action = random.choice(actions)
        p = {state: (random_action, None)}
        value_SDP1 = SDP1.val(t, [p] + ps_tail_1, state)
        value_SDP2 = SDP2.val(t, [p] + ps_tail_2, state)
        policy_SDP1[state] = (random_action, value_SDP1)
        policy_SDP2[state] = (random_action, value_SDP2)
    return policy_SDP1, policy_SDP2

def randomPSGlobal(SDP_parent, SDP1, SDP2, t: int, n: int) -> list[list[dict[State, tuple[Action, float]]]]:
    if n == 0:
        return [], []
    else:
        ps_tail_1, ps_tail_2 = randomPSGlobal(SDP_parent, SDP1, SDP2, t + 1, n - 1)
        p_1, p_2 = randomExtGlobal(SDP_parent, SDP1, SDP2, t, ps_tail_1, ps_tail_2)
        return [p_1] + ps_tail_1, [p_2] + ps_tail_2

def valueCloud(SDP_parent, SDP1, SDP2, t, n, x, n_points):
    x_axis = []
    y_axis = []
    for i in range(n_points):
        ps1, ps2 = randomPSGlobal(SDP_parent, SDP1, SDP2, t, n)
        val_1 = ps1[0][x][1]
        val_2 = ps2[0][x][1]

        # clean_policy = []
        # for p in policy:
        #     clean_p = {state: (action, None) for state, (action, _) in p.items()}
        #     clean_policy.append(clean_p)
        # val_2 = SDP2.val(t, clean_policy, x)
        x_axis.append(val_1)
        y_axis.append(val_2)
    plt.scatter(x_axis, y_axis, c="blue", s=.8)
    plt.show()


test = valueCloud(SDP_parent, SDP1, SDP2, 0, 10, State.DHU, 1000)