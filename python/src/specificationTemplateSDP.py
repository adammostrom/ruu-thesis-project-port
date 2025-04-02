from enum import Enum, auto

import numpy as np
from application.theory import SDP

"""
Declare all states of the SDP below:
"""
class State(Enum):
    # STATE1 = auto()
    # STATE2 = auto()
    # ...
    pass
"""
Declare all actions of the SDP below:
"""
class Action(Enum):
    # ACTION1 = auto()
    # ACTION2 = auto()
    # ...
    pass
"""
Define transition probabilities below:
"""
# pSomething = 0.3
# pSomethingElse = 1- pSomething

class Specification(SDP):
    @property
    def states(self) -> list[State]:
        return list(State)

    # Function that returns the possible actions in any allowed state.
    def actions(self, t: int, x: Enum) -> list[str] | list[None]:
        pass

    # Next takes in timestep t, state x, and action (control) y.
    def nextFunc(self, t: int, x: State, y: State) -> dict[State, float]:
        pass

    def reward(self, t: int, x: State, y: Action, next_x: State) -> int:
        pass

SDP1 = Specification()

