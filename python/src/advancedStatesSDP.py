from enum import Enum, auto

from theory import SDP # Replace 'theory' with 'theoryMemorization' for faster computations.
import numpy as np
"""
Declare all states of the SDP below:
"""
State: TypeAlias = int # Needed because internal functions expect states to be of type 'State'

class State(Enum):
    # Three control-states
    D1 = auto()
    D2 = auto()
    D3 = auto()
    # Six wealth-states
    W0 = auto()
    W1 = auto()
    W2 = auto()
    W3 = auto()
    W4 = auto()
    W5 = auto()
    # Six Carbon-states
    C0 = auto()
    C1 = auto()
    C2 = auto()
    C3 = auto()
    C4 = auto()
    C5 = auto()

    # For example:
    # STATE1 = auto()
    # STATE2 = auto()
    # ...
"""
Declare all actions of the SDP below:
"""
class Action(Enum):
    maxEnv = auto()
    passive = auto()
    maxWealth = auto()
    # For example:
    # ACTION1 = auto()
    # ACTION2 = auto()
    # ...
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
    # the reward of ending up in state 'next_x' in time step 't+1'.
    def reward(self, t: int, x: State, y: Action, next_x: State) -> int:
        pass # Add implementation here...


"""
Instantiate your specification to run its functions.
"""
SDP1 = Specification()

