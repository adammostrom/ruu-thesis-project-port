from enum import Enum
from typing import TypeAlias, Any

"""
This file contains functions for basic checking of the validity of inputs
to the functions used for SDP:s.
"""

class State(Enum):
    pass

class Action(Enum):
    pass

Policy: TypeAlias = dict[State, Any]
PolicySequence: TypeAlias = list[dict[State, Any]]
    
class ErrorChecks():
    def check_t(self, t: int) -> bool:
        if t < 0 or type(t) != int:
            raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")
        else: return True

    def check_n(self, n: int) -> bool:
        if n <= 0 or type(n) != int:
            raise ValueError(f"Invalid time horizon: '{n}' (must be positive integer).")
        else: return True

    def check_x(self, t: int, x: State) -> bool:
        if x not in self.states(t):
            raise ValueError(f"Invalid state: '{x}' at time step '{t}'.")
        else: return True
    
    def check_y(self, t: int, x: State, y: Action) -> bool:
        if y not in self.actions(t, x):
            raise ValueError(f"Invalid action: '{y}' in state '{x}' at time step '{t}'.")
        else: return True

    def check_x_prim(self, t: int, x: State, y: Action, x_prim: State) -> bool:
        if x_prim not in self.nextFunc(t, x, y):
            raise ValueError(f"Invalid next state: '{x_prim}' when taking action '{y}' in state '{x}' at time step '{t}'.")
        else: return True

    def check_a_b(self, a: float, b: float) -> bool:
        if type(a) != float or type(b) != float:
            raise TypeError(f"Inputs must be of type 'float', not '{type(a).__name__}' and '{type(b).__name__}'.")
        else: return True
    
    def check_discount(self) -> bool:
        if type(self.discountRate) != float or self.discountRate < 0:
            raise ValueError(f"Invalid discount rate: '{self.discountRate}' (must be non-negative float).")

    def check_M_Val(self, M_Val: list[tuple[float, float]]) -> bool:
        if type(M_Val) != list or type(M_Val[0]) != tuple or type(M_Val[0][0]) != float or type(M_Val[0][1]) != float:
            raise TypeError(f"Input must be a list of tuples of floats.")
        else: return True
    
    def check_ps(self, ps: PolicySequence) -> bool:
        if not isinstance(ps, list):
            raise TypeError(f"Invalid ps, must be PolicySequence (list of dictionaries, or empty list).")
        if len(ps) > 0:
            if not isinstance(ps[0], dict):
                raise TypeError(f"Invalid ps, must be PolicySequence (list of dictionaries, or empty list).")
        else: return True

    def check_ps_tail(self, ps_tail: PolicySequence) -> bool:
        if not isinstance(ps_tail, list):
            raise TypeError(f"Invalid ps_tail, must be PolicySequence (list of dictionaries, or empty list).")
        if len(ps_tail) > 0:
            if not isinstance(ps_tail[0], dict):
                raise TypeError(f"Invalid ps_tail, must be PolicySequence (list of dictionaries, or empty list).")
        else: return True


    def safe_states(self, t: int) -> list[State]:
        self.check_t(t)
        return self.states(t)

    def safe_actions(self, t: int, x: State) -> list[Action] | list[None]:
        self.check_t(t)
        self.check_x(t, x)
        return self.actions(t, x)

    def safe_nextFunc(self, t: int, x: State, y: Action) -> dict[State, float]:
        self.check_t(t)
        self.check_x(t, x)
        self.check_y(t, x, y)
        return self.nextFunc(t, x, y)

    def safe_reward(self, t: int, x: State, y: Action, x_prim: State) -> float:
        self.check_t(t)
        self.check_x(t, x)
        self.check_y(t, x, y)
        self.check_x_prim(t, x, y, x_prim) 
        return self.reward(t, x, y, x_prim)