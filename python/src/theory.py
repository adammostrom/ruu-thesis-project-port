from abc import ABC, abstractmethod
from enum import Enum
from typing import TypeAlias

import numpy as np

class State(Enum):
    pass

class Action(Enum):
    pass

Policy: TypeAlias = dict[State, Action]
PolicySequence: TypeAlias = list[dict[State, Action]]

# Abstract Base Class (Enforcing required methods)
class SDP(ABC):

    # Returns the value considered as the 'baseline' of specification.
    @property
    @abstractmethod
    def zero(self) -> float:
        pass # Problem-specific, needs to be implemented by user in specification.

    # Returns the discount rate for adding rewards from later time steps 
    # (1 means no discounting takes place).
    @property
    @abstractmethod
    def discountRate(self) -> float:
        pass # Problem-specific, needs to be implemented by user in specification.

    # Returns all states 'x' that are valid in time step 't'.
    @abstractmethod
    def states(self, t: int) -> list[State]:
        pass # Problem-specific, to be implemented by user in specification.

    # Returns all actions 'y' that are valid in time step 't' and state 'x'.
    @abstractmethod
    def actions(self, t: int, x: State) -> list[Action] | list[None]:
        pass # Problem-specific, to be implemented by user in specification.

    # Given a time step 't', a state 'x' and an action 'y', returns the
    # probability distribution over states to be entered in time step 't+1'.
    @abstractmethod
    def nextFunc(self, t: int, x: State, y: Action) -> dict[State, float]:
        pass # Problem-specific, to be implemented by user in specification.

    # Given a time step 't', a state 'x' and an action 'y', returns
    # the reward of ending up in state 'next_x' in time step 't+1'.
    @abstractmethod
    def reward(self, t: int, x: State, y: Action, next_x: State) -> float:
        pass # Problem-specific, to be implemented by user in specification.


    # Checks that input constitutes a valid probability distribution, and
    # if so, returns this as a dictionary.
    def mkSimpleProb(self, pairs: list[tuple[State, float]]) -> dict[State, float]:
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
    
    # Given a time step 't', a policy sequence 'ps' and a state 'x',
    # returns the value of this policy sequence.
    def val(self, t: int, ps: PolicySequence | list[None], x: State) -> float:
        self.check_t(t)
        self.check_ps(ps)
        self.check_x(t, x)

        value = self.zero
        M_vals = list()
        if len(ps) == 0:
            return value
        y = ps[0][x]
        m_next = self.safe_nextFunc(t, x, y)
        for x_prim, pr in m_next.items():
            reward_value = self.safe_reward(t, x, y, x_prim)
            val = self.add(reward_value, self.val(t + 1, ps[1:], x_prim))
            M_vals.append((val, pr))
        value = self.meas(M_vals)
        return value

    # Given a time step 't' and a policy sequence 'ps_tail', returns
    # the best (front) extension to this policy sequence.
    def bestExt(self, t: int, ps_tail: PolicySequence | list[None]) -> Policy:
        self.check_t(t)
        self.check_ps_tail(ps_tail)

        policy = dict()
        for state in self.states(t):
            best_value = -np.inf
            best_action = None
            for action in self.actions(t, state):
                p = {state: action}
                value = self.val(t, [p] + ps_tail, state)
                if value >= best_value:
                    best_value = value
                    best_action = action
            policy[state] = best_action
        return policy

    # Given a time step 't' and a policy sequence 'ps_tail', returns
    # the worst (front) extension to this policy sequence.
    def worstExt(self, t: int, ps_tail: PolicySequence | list[None]) -> Policy:
        self.check_t(t)
        self.check_ps_tail(ps_tail)
        
        policy = dict()
        for state in self.states(t):
            worst_value = np.inf
            worst_action = None
            for action in self.actions(t, state):
                p = {state: action}
                value = self.val(t, [p] + ps_tail, state)
                if value <= worst_value:
                    worst_value = value
                    worst_action = action
            policy[state] = worst_action
        return policy

    # Given a time step 't' and a time horizon 'n', returns an optimal
    # policy sequence of length 'n' starting at time step 't'.
    def bi(self, t: int, n: int) -> PolicySequence:
        self.check_t(t)
        if n == 0: return []
        self.check_n(n)

        ps_tail = self.bi(t + 1, n - 1)
        p = self.bestExt(t, ps_tail)
        return [p] + ps_tail
    
    # Given a time step 't', a time horizon 'n' and a state 'x', returns the
    # optimal action to take at this time and in this state, as well as its value.
    def best(self, t: int, n: int, x: State) -> str:
        self.check_t(t)
        self.check_n(n)
        self.check_x(t, x)

        ps = self.bi(t + 1, n - 1)
        p = self.bestExt(t, ps)
        b = p[x]
        if(b == None):
            b = "No Action"
        vb = self.val(t, [p] + ps, x)
        return f"Horizon, best, value : {n}, {b}, {vb}"
    
    # Given a time step 't', a time horizon 'n' and a state 'x', returns a value
    # between 0 and 1, indicating the "importance" of taking the best action 
    # at this time step in this state considering the current time horizon.
    def mMeas(self, t: int, n: int, x: State) -> float:
        self.check_t(t)
        self.check_n(n)
        self.check_x(t, x)

        ps_tail = self.bi(t+1, n-1)
        p_best = self.bestExt(t, ps_tail)
        p_worst = self.worstExt(t, ps_tail)

        best_action_val = self.val(t, [p_best] + ps_tail, x)
        worst_action_val = self.val(t, [p_worst] + ps_tail, x)
        if best_action_val == self.zero:
            return 0
        return (best_action_val - worst_action_val) / best_action_val
    
    
    # UTILITY FUNCTIONS FOR ERROR HANDLING:
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

    def check_next_x(self, t: int, x: State, y: Action, next_x: State) -> bool:
        if next_x not in self.nextFunc(t, x, y):
            raise ValueError(f"Invalid next state: '{next_x}' when taking action '{y}' in state '{x}' at time step '{t}'.")
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


    def safe_states(self, t: int, x: State) -> list[State]:
        self.check_t(t)
        return self.states(t, x)

    def safe_actions(self, t: int, x: State) -> list[Action] | list[None]:
        self.check_t(t)
        self.check_x(t, x)
        return self.actions(t, x)

    def safe_nextFunc(self, t: int, x: State, y: Action) -> dict[State, float]:
        self.check_t(t)
        self.check_x(t, x)
        self.check_y(t, x, y)
        return self.nextFunc(t, x, y)

    def safe_reward(self, t: int, x: State, y: Action, next_x: State) -> float:
        self.check_t(t)
        self.check_x(t, x)
        self.check_y(t, x, y)
        self.check_next_x(t, x, y, next_x) 
        return self.reward(t, x, y, next_x)