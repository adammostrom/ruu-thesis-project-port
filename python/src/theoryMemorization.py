from abc import ABC, abstractmethod
from enum import Enum, auto

import numpy as np


class State(Enum):
     pass

class Action(Enum):
    pass



# Abstract Base Class (Enforcing required methods)
class SDP(ABC):

    @property
    @abstractmethod
    def states(self) -> list[State]:
        pass

    @abstractmethod
    def actions(self, t: int, x: State) -> list[Action] | list[None]:
        pass # Problem-specific, needs to be implemented by user.

    @abstractmethod
    def nextFunc(self, t: int, x: State, y: Action) -> dict[State, float]:
        pass # Problem-specific, needs to be implemented by user.

    @abstractmethod
    def reward(self, t: int, x: State, y: Action, next_x: State) -> int:
        pass # Problem-specific, needs to be implemented by user.

    # Function that checks that no probabilities are negative, and then
    # returns probabilities for entering all states in next time step.
    def mkSimpleProb(self, pairs: list[tuple[State, float]]) -> dict[State, float]:
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
        
    # Function defining how to add rewards together.
    def add(self, a: float, b: float) -> float:
        if type(a) != float or type(b) != float:
            raise TypeError(f"Inputs must be of type 'float', not '{type(a).__name__}' and '{type(b).__name__}'.")
        return a + b # In default implementation, returns regular floating point addition.
    
    def meas(self, val: float, pr: float) -> float:
        if type(val) != float or type(pr) != float:
            raise TypeError(f"Inputs must be of type 'float', not '{type(val).__name__}' and '{type(pr).__name__}'.")
        return val * pr # In default implementation, returns the expected value.

    def val(self, t: int, ps: list[dict[State, tuple[Action, float]]], x: State) -> float:
        if t < 0 or type(t) != int:
            raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")
        if type(ps) != list:
            raise TypeError(
                f"Invalid policy list, must be list of dictionaries (or empty list)."
            )
        if x not in self.states:
            raise ValueError(f"Invalid state: '{x}'.")
        value = 0.0
        if len(ps) == 0:
            return value
        y = ps[0][x][0]
        m_next = self.nextFunc(t, x, y)
        for x_prim, pr in m_next.items():
            # Added a safe check function, we should to this for each function.
            reward_value = self.safe_reward(t, x, y, x_prim)
            if len(ps) == 1:
                value += self.meas(
                    reward_value, pr
                )
            elif len(ps) > 1 and ps[1][x_prim][1] == None:
                value += self.meas(
                self.add(reward_value, self.val(t + 1, ps[1:], x_prim)),
                pr,
            )
            else:
                value += self.meas(
                    self.add(reward_value, ps[1][x_prim][1]),
                    pr,
                )
        return value

    def bestExt(self, t: int, ps_tail: list[dict[State, tuple[Action, int]]]) -> dict[State, tuple[Action, float]]:
        policy = dict()

        for state in self.states:
            best_value = -np.inf
            best_action = None

            # For each available action in the current state
            for action in self.actions(t, state):
                # Calculate value of taking action in state
                p = {state: (action, None)}
                value = self.val(t, [p] + ps_tail, state)
                # Choose the action with the highest expected value
                if value >= best_value:
                    best_value = value
                    best_action = action

            policy[state] = (best_action, best_value)

        return policy

    def worstExt(self, t: int, ps_tail: list[dict[State, tuple[Action, float]]] | list[None]) -> dict[State, Action]:
        if t < 0 or type(t) != int:
            raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")
        if type(ps_tail) != list:
            raise TypeError(f"Invalid ps_tail, must be list of dictionaries (or empty list).")
        
        policy = dict()

        for state in self.states:
            worst_value = np.inf
            worst_action = None

            # For each available action in the current state
            for action in self.actions(t, state):
                # Calculate value of taking action in state
                p = {state: (action, None)}
                value = self.val(t, [p] + ps_tail, state)
                # Choose the action with the lowest expected value
                if value <= worst_value:
                    worst_value = value
                    worst_action = action

            policy[state] = (worst_action, worst_value)

        return policy
    
    def bi(self, t: int, n: int) -> list[dict[State, tuple[Action, float]]]:
        if n == 0:
            return []
        else:
            ps_tail = self.bi(t + 1, n - 1)
            p = self.bestExt(t, ps_tail)
            return [p] + ps_tail
            
    def best(self, t: int, n: int, x: State) -> str:
        if n <= 0:
            raise ValueError("The horizon must be greater than zero!")
        ps = self.bi(t + 1, n - 1)
        p = self.bestExt(t, ps)
        b = p[x][0]
        # Added the return of No Action instead of None as a string, mainly for testing purposes.
        if(b == None):
            b = "No Action"
        vb = self.val(t, [p] + ps, x)
        return f"Horizon, best, value : {n}, {b}, {vb}"
    
    # Returns a value between 0 and 1, where 0 means "does not matter at all"
    # and 1 means "matters maximally" to achieving the defined goal of the SDP.
    def mMeas(self, t: int, n: int, x: State) -> float:
        if t < 0 or type(t) != int:
            raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")
        if n < 0 or type(n) != int:
            raise ValueError(f"Invalid time horizon: '{n}' (must be positive integer).")
        if x not in self.states:
            raise ValueError(f"Invalid state: '{x}'.")
        ps_tail = self.bi(t+1, n-1)
        p_best = self.bestExt(t, ps_tail)
        p_worst = self.worstExt(t, ps_tail)

        best_action_val = self.val(t, [p_best] + ps_tail, x)
        worst_action_val = self.val(t, [p_worst] + ps_tail, x)
        if best_action_val == 0:
            return 0
        return (best_action_val - worst_action_val) / best_action_val
    
    
    # UTILITY FUNCTIONS FOR ERROR HANDLING:
    
    def safe_reward(self, t: int, x: State, y: Action, next_x: State) -> float:
        self.check_reward(t, x, y, next_x)  
        return self.reward(t, x, y, next_x)  
    
    def check_reward(self, t: int, x: State, y: Action, next_x: State) -> bool:
        if t < 0 or type(t) != int:
            raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")
        if x not in self.states:
            raise ValueError(f"Invalid state: '{x}'")
        if y not in self.actions(t, x):
            raise ValueError(f"Invalid action: '{y}'")
        if next_x not in self.states:
            raise ValueError(f"Invalid next state: '{next_x}'")
        else: return True
