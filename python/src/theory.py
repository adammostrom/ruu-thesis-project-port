from abc import ABC, abstractmethod
from enum import Enum

import numpy as np


class State(Enum):
    pass

class Action(Enum):
    pass


# Abstract Base Class (Enforcing required methods)
class SDP(ABC):

    # Returns the value considered as the 'baseline' of specification.
    @property
    @abstractmethod
    def zero(self) -> float:
        pass # Problem-specific, needs to be implemented by user in specification.

    # Returns all states 'x' that are valid in time step 't'.
    @abstractmethod
    def states(self, t: int) -> list[State]:
        pass # Problem-specific, to be implemented by user in specification.

    # Returns all actions 'y' that are valid in time step 't'
    # and state 'x'.
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
    def reward(self, t: int, x: State, y: Action, next_x: State) -> int:
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
        if type(a) != float or type(b) != float:
            raise TypeError(f"Inputs must be of type 'float', not '{type(a).__name__}' and '{type(b).__name__}'.")
        return a + b
    
    # Given a probability distribution over values, returns the expected value.
    def meas(self, M_Val: list[tuple[float, float]]) -> float:
        if type(M_Val) != list or type(M_Val[0]) != tuple or type(M_Val[0][0]) != float or type(M_Val[0][0]) != float:
            raise TypeError(f"Input must be a list of tuples of floats.")
        exp_val = 0
        for val, pr in M_Val:
            exp_val += val * pr
        return exp_val
    
    # Given a time step 't', a policy sequence 'ps' and a state 'x',
    # returns the value of this policy sequence.
    def val(self, t: int, ps: list[dict[State, Action]], x: State) -> float:
        if t < 0 or type(t) != int:
            raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")
        if type(ps) != list:
            raise TypeError(f"Invalid policy list, must be list of dictionaries (or empty list).")
        if x not in self.states(t):
            raise ValueError(f"Invalid state: '{x}'")
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
    def bestExt(self, t: int, ps_tail: list[dict[State, Action]]) -> dict[State, Action]:
        if t < 0 or type(t) != int:
            raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")
        if type(ps_tail) != list:
            raise TypeError(f"Invalid ps_tail, must be list of dictionaries (or empty list).")

        policy = dict()

        for state in self.states(t):
            best_value = -np.inf
            best_action = None

            # For each available action in the current state
            for action in self.actions(t, state):
                # Calculate value of taking action in state
                p = {state: action}
                value = self.val(t, [p] + ps_tail, state)
                # Choose the action with the highest expected value
                if value >= best_value:
                    best_value = value
                    best_action = action

            policy[state] = best_action

        return policy

    # Given a time step 't' and a policy sequence 'ps_tail', returns
    # the worst (front) extension to this policy sequence.
    def worstExt(self, t: int, ps_tail: list[dict[State, Action]] | list[None]) -> dict[State, Action]:
        if t < 0 or type(t) != int:
            raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")
        if type(ps_tail) != list:
            raise TypeError(f"Invalid ps_tail, must be list of dictionaries (or empty list).")
        
        policy = dict()

        for state in self.states(t):
            worst_value = np.inf
            worst_action = None

            # For each available action in the current state
            for action in self.actions(t, state):
                # Calculate value of taking action in state
                p = {state: action}
                value = self.val(t, [p] + ps_tail, state)
                # Choose the action with the lowest expected value
                if value <= worst_value:
                    worst_value = value
                    worst_action = action

            policy[state] = worst_action

        return policy

    # Given a time step 't' and a time horizon 'n', returns an optimal
    # policy sequence of length 'n' starting at time step 't'.
    def bi(self, t: int, n: int) -> list[dict[State, Action]]:
        if t < 0 or type(t) != int:
            raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")
        if n < 0 or type(n) != int:
            raise ValueError(f"Invalid time horizon: '{n}' (must be positive integer).")
        if n == 0:
            return []
        else:
            ps_tail = self.bi(t + 1, n - 1)
            p = self.bestExt(t, ps_tail)
            return [p] + ps_tail
    
    # Given a time step 't', a time horizon 'n' and a state 'x', returns the
    # optimal action to take at this time and in this state, as well as its value.
    def best(self, t: int, n: int, x: State) -> str:
        if t < 0 or type(t) != int:
            raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")
        if n <= 0:
            raise ValueError("Time horizon must be greater than zero!")
        if n < 0 or type(n) != int:
            raise ValueError(f"Invalid time horizon: '{n}' (must be positive integer).")
        if x not in self.states(t):
            raise ValueError(f"Invalid state: '{x}'.")
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
        if t < 0 or type(t) != int:
            raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")
        if n < 0 or type(n) != int:
            raise ValueError(f"Invalid time horizon: '{n}' (must be positive integer).")
        if x not in self.states(t):
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
    def safe_states(self, t: int, x: State) -> list[State]:
        self.check_states(t, x)
        return self.states(t, x)

    def check_states(self, t: int, x: State) -> bool:
        if t < 0 or type(t) != int:
            raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")
        else: return True

    def safe_actions(self, t: int, x: State) -> list[Action] | list[None]:
        self.check_actions(t, x)
        return self.actions(t, x)

    def check_actions(self, t: int, x: State) -> bool:
        if t < 0 or type(t) != int:
            raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")
        if x not in self.states(t):
            raise ValueError(f"Invalid state: '{x}'.")
        else: return True

    def safe_nextFunc(self, t: int, x: State, y: Action) -> dict[State, float]:
        self.check_nextFunc(t, x, y)
        return self.nextFunc(t, x, y)

    def check_nextFunc(self, t: int, x: State, y: Action) -> bool:
        if t < 0 or type(t) != int:
            raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")
        if x not in self.states(t):
            raise ValueError(f"Invalid state: '{x}'.")
        if y not in self.actions(t, x):
            raise ValueError(f"Invalid action: '{y}'.")
        else: return True

    def safe_reward(self, t: int, x: State, y: Action, next_x: State) -> float:
        self.check_reward(t, x, y, next_x)  
        return self.reward(t, x, y, next_x)  

    def check_reward(self, t: int, x: State, y: Action, next_x: State) -> bool:
        if t < 0 or type(t) != int:
            raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")
        if x not in self.states(t):
            raise ValueError(f"Invalid state: '{x}'.")
        if y not in self.actions(t, x):
            raise ValueError(f"Invalid action: '{y}'.")
        if next_x not in self.states(t):
            raise ValueError(f"Invalid next state: '{next_x}'.")
        else: return True