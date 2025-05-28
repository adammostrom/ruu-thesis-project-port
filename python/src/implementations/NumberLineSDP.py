from enum import Enum, auto
from typing import TypeAlias

# from src.application.theory import SDP # To run SDP framework without memoization
from python.src.application.theoryMemoization import SDP

"""
Specification of a number line SDP with no explicit limit on the number of states.
A limitation to this implementation is that at time step zero, one must always start 
in state 'ZERO', and for any given time step 't', one must be in a state between 
'NEGt' and 'POSt'. 
"""

# Declare all states of the SDP below:
State: TypeAlias = str # Needed because internal functions expect states to be of type 'State'
def generate_states(t: int) -> list[str]:
    return [str(i) for i in range(-t-1, t+2)]

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
        states = self.states(t+1)
        if x not in states:
            raise ValueError(f"Invalid state: '{x}' for time step '{t}'.")
        left = str(int(x)-1)
        current = x
        right = str(int(x)+1)
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
    
    def help(self):
        EXPOSED_METHODS = {
            "states    ": "states(t: int) -> list[State]",
            "actions   ": "actions(t: int, x: State) -> list[Action] | list[None]",
            "nextFunc  ": "nextFunc(t: int, x: State, y: Action) -> dict[State, float]",
            "reward    ": "reward(t: int, x: State, y: Action, x_prim: State) -> float"
        }

        print("NumberLine SDP module")
        print("This module defines a simple stochastic dynamic program on an infinite number line.")
        print("States are integers (as strings), actions move left/right/stay, and rewards depend on the resulting state.\n")

        print("Available functions:")
        for name, sig in EXPOSED_METHODS.items():
            print(f"  • {sig}")

        print("\nReward interpretation:")
        print("  • Reward is equal to the resulting state (as float).")
        print("    Higher-numbered states yield higher rewards.\n")

        print("Use with:")
        print("  • SDP1 : instance of NumberLine for immediate use or testing.\n")

        print("Notes:")
        print("  • States are strings: e.g., '0', '-1', '2'")
        print("  • Must start in state '0' at t = 0")
        print("  • At time t, valid states are between -t-1 and t+1 inclusive")



SDP1 = NumberLine()
