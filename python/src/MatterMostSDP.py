from enum import Enum, auto
import numpy as np
from theory import SDP

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


    # Next takes in timestep t, state x, and action (control) y.
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
        # Value is added for transitioning into states which do not have low economic
        # output and at the same time are not comitted to severe future climate change.
        if t < 0 or type(t) != int:
            raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")
        if x not in self.states:
            raise ValueError(f"Invalid state: '{x}'")
        if y not in self.actions(t, x):
            raise ValueError(f"Invalid action: '{y}'")
        if next_x not in self.states:
            raise ValueError(f"Invalid next state: '{next_x}'")
        return 1.0 if next_x in [State.DHU, State.SHU] else 0.0


# First quick check that program still produces same results as before generalization.
SDP1 = MatterMost()
# result = SDP1.nextFunc(0, State.DHU, Action.Start)
# result = SDP1.reward(0, State.DHU, Action.Start, State.DHU)
# result = SDP1.bi(0, 3)
# result = SDP1.best(0, 7, State.DHU)
result = SDP1.mMeas(3, 7, State.DHU)
print(result)

