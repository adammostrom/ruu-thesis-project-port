from enum import Enum, auto
from typing import TypeAlias

from theoryMemorization import SDP

"""
Specification of a number line SDP with no explicit limit on the number of states.
A limitation to this implementation is that at time step zero, one must always start 
in state 'ZERO', and for any given time step 't', one must be in a state between 
'NEGt' and 'POSt'. 
"""

# Declare all states of the SDP below:
State: TypeAlias = int # Needed because internal functions expect states to be of type 'State'
def generate_states(t: int) -> list[int]:
    return [i for i in range(-t, t+1)]

# Declare all actions of the SDP below:
class Action(Enum):
    Left = auto()
    Right = auto()
    Stay = auto()

# Define transition probabilities below:
pL_Left = 0.85
pS_Left = 0.1
pR_Left = 0.05

pL_Stay = 0.1
pS_Stay = 0.8
pR_Stay = 0.1

pL_Right = 0.05
pS_Right = 0.1
pR_Right = 0.85

class NumberLine(SDP):

    @property
    def zero(self) -> float:
        return 0.0

    @property
    def discountRate(self) -> float:
        return 1.0
    
    def states(self, t: int) -> list[State]:
        return list(generate_states(t))

    def actions(self, t: int, x: State) -> list[Action] | list[None]:
        if x in self.states(t):
            return [Action.Left, Action.Right, Action.Stay]
        else:
            raise ValueError(f"Invalid State: '{x}' for time step '{t}'.")

    def nextFunc(self, t: int, x: State, y: Action) -> dict[State, float]:
        states = self.states(t)
        if x not in states:
            raise ValueError(f"Invalid state: '{x}' for time step '{t}'.")
        left = x-1
        current = x
        right = x+1
        match y:
            case Action.Left:
                transitions = [(left, pL_Left), (current, pS_Left), (right, pR_Left)]
            case Action.Stay:
                transitions = [(left, pL_Stay), (current, pS_Stay), (right, pR_Stay)]
            case Action.Right:
                transitions = [(left, pL_Right), (current, pS_Right), (right, pR_Right)]
            case _:
                raise ValueError(f"Unknown action: {y}")
        return self.mkSimpleProb(transitions)

    def reward(self, t: int, x: State, y: Action, x_prim: State) -> float:
        return float(x_prim)



SDP1 = NumberLine()

# result = SDP1.states(2)
# result = SDP1.actions(3, 1)
# result = SDP1.nextFunc(4, 3, Action.Stay)

# ps = SDP1.bi(0, 3)
# result = SDP1.val(0, ps, 0)

# result = SDP1.safe_reward(3, 3, Action.Stay, 4)
# result = SDP1.bi(0, 3)
# result = SDP1.best(0, 7, 0)
result = SDP1.mMeas(0, 1, 0)

print(result)