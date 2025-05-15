from enum import Enum
from typing import TypeAlias

"""
This file contains functions that are used by functions in sequential decision problems,
but which are in themselves more general and can be used outside of SDP:s.
"""

class State(Enum):
    pass

class Action(Enum):
    pass
    
class MathOperations():
    # Checks that input constitutes a valid probability distribution, and
    # if so, returns this as a dictionary.
    def mkSimpleProb(self, pairs: list[tuple[State, float]]) -> dict[State, float]:
        # TODO also check for an empty list before indexing into pairs[0] (... "non-empty list of" below)
        if type(pairs) != list or type(pairs[0]) != tuple:
            raise TypeError(f"Input must be a list of tuples.")
        pr_sum = 0
        dist = {}
        for st, pr in pairs:
            if pr >= 0:
                dist[st] = pr
                pr_sum += pr
            else:
                raise ValueError("No negative probabilities allowed.")
        if abs(pr_sum - 1) < 1e-7:
            return dist
        else:
            raise ValueError(f"Probabilities do not sum to 1 but instead to '{pr_sum}'.")
        
    # Given two float numbers 'a' and 'b', returns their sum.
    def add(self, a: float, b: float) -> float:
        self.check_a_b(a, b)
        self.check_discount()
        return a + b * self.discountRate
    
    # Given a probability distribution over values, returns the expected value.
    def meas(self, M_Val: list[tuple[float, float]]) -> float:
        self.check_M_Val(M_Val)
        exp_val = 0
        for val, pr in M_Val:
            exp_val += val * pr
        return exp_val
