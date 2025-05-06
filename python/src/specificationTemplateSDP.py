from enum import Enum, auto

import numpy as np
from application.theory import SDP

"""
Declare all states of the SDP below:
"""
class State(Enum):
    # For example:
    # STATE1 = auto()
    # STATE2 = auto()
    # ...
    pass
"""
Declare all actions of the SDP below:
"""
class Action(Enum):
    # For example:
    # ACTION1 = auto()
    # ACTION2 = auto()
    # ...
    pass
"""
Define transition probabilities below:
"""
# For example:
# pSomething = 0.3
# pSomethingElse = 1- pSomething

class Specification(SDP):

    # Returns the value considered as the 'baseline' of specification.
    @property
    def zero(self) -> float:
        pass # Add implementation here...

    # Returns the discount rate for adding rewards from later time steps 
    # (1 means no discounting takes place).
    @property
    def discountRate(self) -> float:
        pass # Problem-specific, needs to be implemented by user in specification.
    
    # Returns all states 'x' that are valid in time step 't'.
    def states(self, t: int) -> list[State]:
        pass # Add implementation here...

    # Returns all actions 'y' that are valid in time step 't'
    # and state 'x'.
    def actions(self, t: int, x: State) -> list[Action] | list[None]:
        pass # Add implementation here...

    # Given a time step 't', a state 'x' and an action 'y', returns the
    # probability distribution over states to be entered in time step 't+1'.
    def nextFunc(self, t: int, x: State, y: State) -> dict[State, float]:
        pass # Add implementation here...

    # Given a time step 't', a state 'x' and an action 'y', returns
    # the reward of ending up in state 'x_prim' in time step 't+1'.
    def reward(self, t: int, x: State, y: Action, x_prim: State) -> int:
        pass # Add implementation here...


"""
Instantiate your specification to run its functions.
"""
SDP1 = Specification()

