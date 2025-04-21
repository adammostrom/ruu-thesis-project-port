import copy
from enum import Enum, auto

import numpy as np
from src.application.theory import SDP


# Defining the set of allowed states in the SDP.
class State (Enum):
    NEG2 = -2
    NEG1 = -1
    ZERO = 0
    POS1 = 1
    POS2 = 2

class Action (Enum):
    Left = auto()
    Right = auto()
    Stay = auto()

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

class NumberLineSDP (SDP):
    
    @property
    def states(self) -> list[State]:
        return list(State)

    
    


    # Function that returns the possible actions in any allowed state.
    def actions(self, x: str) -> list[str] | list[None]:
        if x in self.states:
            return [Action.Left, Action.Right, Action.Stay]
        else:
            raise ValueError(f"Invalid State: '{x}'.")
        
    # For a current time step, state and action, returns the probabilities of entering each state in the next time step.
    def nextFunc(self, t: int, x: State, y: Action) -> dict[State, float]:
        # Actions for all states are "Left", "Stay", or "Right".
            if x == State._-2:
                if y == Action.Left:
                    return self.mkSimpleProb(
                        [
                            (State._-2, pL_Left + pS_Left),
                            (State._-1, pR_Left)
                        ]
                    )
                elif y == Action.Stay:
                    return self.mkSimpleProb(
                        [
                            (State._-2, pL_Stay + pS_Stay),
                            (State._-1, pR_Stay)
                        ]
                    )
                elif y == Action.Right:
                    return self.mkSimpleProb(
                        [
                            (State._-2, pL_Right + pS_Right),
                            (State._-1, pR_Right)
                        ]
                    )
                else:
                    raise ValueError("Invalid control for state=-2.")
            elif x == State._-1:
                if y == Action.Left:
                    return self.mkSimpleProb(
                        [
                            (State._-2, pL_Left),
                            (State._-1, pS_Left),
                            (State._0,  pR_Left)
                        ]
                    )
                elif y == Action.Stay:
                    return self.mkSimpleProb(
                        [
                            (State._-2, pL_Stay),
                            (State._-1, pS_Stay),
                            (State._0, pR_Stay)
                        ]
                    )
                elif y == Action.Right:
                    return self.mkSimpleProb(
                        [
                            (State._-2, pL_Right),
                            (State._-1, pS_Right),
                            (State._0, pR_Right)
                        ]
                    )
                else:
                    raise ValueError("Invalid control for state=-1.")
            elif x == State._0:
                if y == Action.Left:
                    return self.mkSimpleProb(
                        [
                            (State._-1, pL_Left),
                            (State._0, pS_Left),
                            (State._1,  pR_Left)
                        ]
                    )
                elif y == Action.Stay:
                    return self.mkSimpleProb(
                        [
                            (State._-1, pL_Stay),
                            (State._0, pS_Stay),
                            (State._1, pR_Stay)
                        ]
                    )
                elif y == Action.Right:
                    return self.mkSimpleProb(
                        [
                            (State._-1, pL_Right),
                            (State._0, pS_Right),
                            (State._1, pR_Right)
                        ]
                    )
                else:
                    raise ValueError("Invalid control for state=0.")
            elif x == State._1:
                if y == Action.Left:
                    return self.mkSimpleProb(
                        [
                            (State._0, pL_Left),
                            (State._1, pS_Left),
                            ("2", pR_Left)
                        ]
                    )
                elif y == "Stay":
                    return self.mkSimpleProb(
                        [
                            (State._0, pL_Stay),
                            (State._1, pS_Stay),
                            ("2", pR_Stay)
                        ]
                    )
                elif y == "Right":
                    return self.mkSimpleProb(
                        [
                            (State._0, pL_Right),
                            (State._1, pS_Right),
                            ("2", pR_Right)
                        ]
                    )
                else:
                    raise ValueError("Invalid control for state=1.")
            elif x == "2":
                if y == Action.Left:
                    return self.mkSimpleProb(
                        [
                            (State._1, pL_Left),
                            ("2", pS_Left + pR_Left),
                        ]
                    )
                elif y == "Stay":
                    return self.mkSimpleProb(
                        [
                            (State._1, pL_Stay),
                            ("2", pS_Stay + pR_Stay)
                        ]
                    )
                elif y == "Right":
                    return self.mkSimpleProb(
                        [
                            (State._1, pL_Right),
                            ("2", pS_Right + pR_Right)
                        ]
                    )
                else:
                    raise ValueError("Invalid control for state=2.")
            else:
                raise ValueError(f"Invalid state: {x}")
        
    # Reward function.
    def reward(t: int, x: str, y: str, next_x: str) -> float:
        # Value is added for transitioning into states which do not have low economic
        # output and at the same time are not comitted to severe future climate change.
        if t < 0 or type(t) != int:
            raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")
        if x not in states:
            raise ValueError(f"Invalid state: '{x}'")
        if y not in actions(x):
            raise ValueError(f"Invalid action: '{y}'")
        if next_x not in states:
            raise ValueError(f"Invalid next state: '{next_x}'")
        return 1.0 if next_x == "2" else 0.0
