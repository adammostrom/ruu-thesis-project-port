import copy

import numpy as np
from theory import TheoryInterface

# Defining the probabilities for the transition function.
pL_Left = 0.85
pS_Left = 0.1
pR_Left = 0.05

pL_Stay = 0.1
pS_Stay = 0.8
pR_Stay = 0.1

pL_Right = 0.05
pS_Right = 0.1
pR_Right = 0.85


class NumberLineSDP(TheoryInterface):

    @property
    def states(self):
        return ["-2", "-1", "0", "1", "2"]

    # Function that returns the possible actions in any allowed state.
    def actions(self, x: str) -> list[str] | list[None]:
        if x in self.states:
            return ["Left", "Right", "Stay"]
        else:
            raise ValueError(f"Invalid State: '{x}'.")

    # For a current time step, state and action, returns the probabilities of entering each state in the next time step.
    def next(self, t: int, x: str, y: str) -> dict[str, float]:
        # Actions for all states are "Left", "Stay", or "Right".
        if x == "-2":
            if y == "Left":
                return self.mkSimpleProb([("-2", pL_Left + pS_Left), ("-1", pR_Left)])
            elif y == "Stay":
                return self.mkSimpleProb([("-2", pL_Stay + pS_Stay), ("-1", pR_Stay)])
            elif y == "Right":
                return self.mkSimpleProb(
                    [("-2", pL_Right + pS_Right), ("-1", pR_Right)]
                )
            else:
                raise ValueError("Invalid control for state=-2.")
        elif x == "-1":
            if y == "Left":
                return self.mkSimpleProb(
                    [("-2", pL_Left), ("-1", pS_Left), ("0", pR_Left)]
                )
            elif y == "Stay":
                return self.mkSimpleProb(
                    [("-2", pL_Stay), ("-1", pS_Stay), ("0", pR_Stay)]
                )
            elif y == "Right":
                return self.mkSimpleProb(
                    [("-2", pL_Right), ("-1", pS_Right), ("0", pR_Right)]
                )
            else:
                raise ValueError("Invalid control for state=-1.")
        elif x == "0":
            if y == "Left":
                return self.mkSimpleProb(
                    [("-1", pL_Left), ("0", pS_Left), ("1", pR_Left)]
                )
            elif y == "Stay":
                return self.mkSimpleProb(
                    [("-1", pL_Stay), ("0", pS_Stay), ("1", pR_Stay)]
                )
            elif y == "Right":
                return self.mkSimpleProb(
                    [("-1", pL_Right), ("0", pS_Right), ("1", pR_Right)]
                )
            else:
                raise ValueError("Invalid control for state=0.")
        elif x == "1":
            if y == "Left":
                return self.mkSimpleProb(
                    [("0", pL_Left), ("1", pS_Left), ("2", pR_Left)]
                )
            elif y == "Stay":
                return self.mkSimpleProb(
                    [("0", pL_Stay), ("1", pS_Stay), ("2", pR_Stay)]
                )
            elif y == "Right":
                return self.mkSimpleProb(
                    [("0", pL_Right), ("1", pS_Right), ("2", pR_Right)]
                )
            else:
                raise ValueError("Invalid control for state=1.")
        elif x == "2":
            if y == "Left":
                return self.mkSimpleProb(
                    [
                        ("1", pL_Left),
                        ("2", pS_Left + pR_Left),
                    ]
                )
            elif y == "Stay":
                return self.mkSimpleProb([("1", pL_Stay), ("2", pS_Stay + pR_Stay)])
            elif y == "Right":
                return self.mkSimpleProb([("1", pL_Right), ("2", pS_Right + pR_Right)])
            else:
                raise ValueError("Invalid control for state=2.")
        else:
            raise ValueError(f"Invalid state: {x}")

    # Reward function.
    def reward(self, t: int, x: str, y: str, next_x: str) -> int:
        # Value is added for transitioning into states which do not have low economic
        # output and at the same time are not comitted to severe future climate change.
        if t < 0 or type(t) != int:
            raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")
        if x not in self.states:
            raise ValueError(f"Invalid state: '{x}'")
        if y not in self.actions(x):
            raise ValueError(f"Invalid action: '{y}'")
        if next_x not in self.states:
            raise ValueError(f"Invalid next state: '{next_x}'")
        return 1.0 if next_x == "2" else 0.0

    # Returns a value between 0 and 1, where 0 means "does not matter at all"
    # and 1 means "matters maximally" to achieving the defined goal of the SDP.
    def mMeas(self, t: int, n: int, x: str) -> float:
        if x in ["SHU", "SHC", "SLU", "SLC"]:
            return 0
        else:
            ps = self.bi(t, n)
            ps_prim = copy.deepcopy(ps)
            if ps[0][x] == "Start":
                ps_prim[0][x] = "Delay"
            else:
                ps_prim[0][x] = "Start"

            best_action_val = self.val(t, ps, x)
            worst_action_val = self.val(t, ps_prim, x)

            return (best_action_val - worst_action_val) / best_action_val
