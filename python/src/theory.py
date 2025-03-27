import copy
from abc import ABC, abstractmethod
from enum import Enum

import numpy as np


# Base Enums (can be expanded per spec)
class State(Enum):
    pass


class Action(Enum):
    pass


# Abstract Base Class (Enforcing required methods)
class TheoryInterface(ABC):


    @abstractmethod
    def next(t: int, x: str, y: str) -> dict[str, float]:
        pass

    @abstractmethod
    def reward(t: int, x: str, y: str, next_x: str) -> int:
        pass

    @abstractmethod
    def mMeas(t: int, n: int, x: str) -> float:
        pass

    @abstractmethod
    def meas(val: float, pr: float) -> float:
        pass

    @abstractmethod
    def actions(x: str) -> list[str] | list[None]:
        pass

    @property
    @abstractmethod
    def states(self) -> list[State]:
        pass



    # Function that checks that no probabilities are negative, and then
    # returns probabilities for entering all states in next time step.
    def mkSimpleProb(self, pairs: list[tuple[str, float]]) -> dict[str, float]:
        dist: dict[str, float] = {}
        for st, pr in pairs:
            if pr >= 0:
                dist[st] = pr
        return dist

    # TODO:
    # Up for discussion: Do we want to make Actions into a datastructure like State is?
    # That is how it is implemented in Haskell atleast, this would allow us to remove the hardcoded strings in the code.
    # Actually, we should definitly do this. 
    """ @property
    @abstractmethod
    def actions(self) -> list[Action]:
        pass """
        
        # Function defining how to add rewards together.
    def add(self, a: float, b: float) -> float:
        if type(a) != float or type(b) != float:
            raise TypeError(f"Inputs must be of type 'float', not '{type(a).__name__}' and '{type(b).__name__}'.")
        return a + b # In default implementation, returns regular floating point addition.
    
    def meas(self, val: float, pr: float) -> float:
        if type(val) != float or type(pr) != float:
            raise TypeError(f"Inputs must be of type 'float', not '{type(val).__name__}' and '{type(pr).__name__}'.")
        return val * pr # In default implementation, returns the expected value.

    def val(self, t: int, ps: list[dict[str, str]], x: str) -> float:
        if t < 0 or type(t) != int:
            raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")
        if type(ps) != list:
            raise TypeError(
                f"Invalid policy list, must be list of dictionaries (or empty list)."
            )
        if x not in self.states:
            raise ValueError(f"Invalid state: '{x}'")
        value = 0.0
        if len(ps) == 0:
            return value
        y = ps[0][x]
        m_next = self.next(t, x, y)
        for x_prim, pr in m_next.items():
            value += self.meas(
                self.add(self.reward(t, x, y, x_prim), self.val(t + 1, ps[1:], x_prim)),
                pr,
            )
        return value

    def bestExt(self, t: int, ps_tail: list[dict[str, str]]) -> dict[str, str]:
        policy = dict()

        for state in self.states:
            best_value = -np.inf
            best_action = None

            # For each available action in the current state
            for action in self.actions(state):
                # Calculate value of taking action in state
                p = {state: action}
                value = self.val(t, [p] + ps_tail, state)
                # Choose the action with the highest expected value
                if value >= best_value:
                    best_value = value
                    best_action = action

            policy[state] = best_action

        return policy

    def bi(self, t: int, n: int) -> list[dict[str, str]]:
        if n == 0:
            return []
        else:
            ps_tail = self.bi(t + 1, n - 1)
            p = self.bestExt(t, ps_tail)
            return [p] + ps_tail
        
    def best(self, t: int, n: int, x: str) -> str:
        if n <= 0:
            raise ValueError("The horizon must be greater than zero!")
        ps = self.bi(t + 1, n - 1)
        p = self.bestExt(t, ps)
        b = p[x]
        # Added the return of No Action instead of None as a string, mainly for testing purposes.
        if(b == None):
            b = "No Action"
        vb = self.val(t, [p] + ps, x)
        return f"Horizon, best, value : {n}, {b}, {vb}"
