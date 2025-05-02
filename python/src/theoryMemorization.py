from abc import ABC, abstractmethod
import matplotlib.pyplot as plt
from typing import TypeAlias
from enum import Enum
import numpy as np
import random

from errorChecks import ErrorChecks
from mathOperations import MathOperations


"""
This file contains a modified version of the SDP class found in theory.py. It is different
in that it modifies policy generation to store not just 'State'-'Action'-pairs, but instead
'State'-'(Action, Value)'-pairs (see updated type aliases below), where 'Value' is the value 
of taking 'Action' in 'State' given the current policy sequence. This approach allows for 
greatly increased computation speeds, in turn allowing for much longer policy sequences and 
SDP implementations with larger space / action-spaces. 

The modifications compared with the SDP class from theory.py are mainly concentrated in the
function 'val'. The functions 'bestExt', 'worstExt' and 'best' have been tweaked to fit the
new policy-type, but otherwise function identically to the original.
"""

class State(Enum):
    pass

class Action(Enum):
    pass

Policy: TypeAlias = dict[State, tuple[Action, float]]
PolicySequence: TypeAlias = list[dict[State, tuple[Action, float|None]]]

# Abstract Base Class (Enforcing required methods)
class SDP(ABC, ErrorChecks, MathOperations):

    # Returns the value considered as the 'baseline' of specification.
    @property
    @abstractmethod
    def zero(self) -> float:
        pass # Problem-specific, to be implemented by user in specification.

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
    # the reward of ending up in state 'x_prim' in time step 't+1'.
    @abstractmethod
    def reward(self, t: int, x: State, y: Action, x_prim: State) -> float:
        pass # Problem-specific, to be implemented by user in specification.


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
        y = ps[0][x][0]
        m_next = self.safe_nextFunc(t, x, y)
        for x_prim, pr in m_next.items():
            reward = self.safe_reward(t, x, y, x_prim)
            if len(ps) == 1:
                val = reward
            elif len(ps) > 1 and ps[1][x_prim][1] == None:
                val = self.add(reward, self.val(t + 1, ps[1:], x_prim))
            else:
                val = self.add(reward, ps[1][x_prim][1])
            M_vals.append((val, pr))
        value = self.meas(M_vals)
        return value

    # Given a time step 't' and a policy sequence 'ps_tail', returns
    # the best (front) extension to this policy sequence.
    def bestExt(self, t: int, ps_tail: PolicySequence) -> Policy:
        self.check_t(t)
        self.check_ps_tail(ps_tail)
        
        policy = dict()
        for state in self.states(t):
            best_value = -np.inf
            best_action = None
            for action in self.actions(t, state):
                p = {state: (action, None)}
                value = self.val(t, [p] + ps_tail, state)
                if value >= best_value:
                    best_value = value
                    best_action = action
            policy[state] = (best_action, best_value)
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
                p = {state: (action, None)}
                value = self.val(t, [p] + ps_tail, state)
                if value <= worst_value:
                    worst_value = value
                    worst_action = action
            policy[state] = (worst_action, worst_value)
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
        b = p[x][0]
        if(b == None):
            b = "No Action"
        vb = self.val(t, [p] + ps, x)
        return f"Horizon, best, value : {n}, {b}, {vb}"
    
    # Given a time step 't', a time horizon 'n' and a state 'x', returns the
    # least optimal action to take at this time and in this state, as well as its value.
    def worst(self, t: int, n: int, x: State) -> str:
        self.check_t(t)
        self.check_n(n)
        self.check_x(t, x)

        ps = self.bi(t + 1, n - 1)
        p = self.worstExt(t, ps)
        b = p[x][0]
        if(b == None):
            b = "No Action"
        vb = self.val(t, [p] + ps, x)
        return f"Horizon, worst, value : {n}, {b}, {vb}"
    
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
    

    """
    Below are functions that are not included in the original model from Idris.
    """
    # Given a time step 't' and a policy sequence 'ps_tail', returns
    # a random extension to this policy sequence.
    def randomExt(self, t: int, ps_tail: PolicySequence) -> Policy:
        self.check_t(t)
        self.check_ps_tail(ps_tail)

        policy = dict()
        for state in self.states(t):
            actions = self.actions(t, state)
            random_action = random.choice(actions)
            p = {state: (random_action, None)}
            value = self.val(t, [p] + ps_tail, state)
            policy[state] = (random_action, value)
        return policy

    # Given a time step 't' and a time horizon 'n', returns a random
    # policy sequence of length 'n' starting at time step 't'.
    def randomPS(self, t: int, n: int) -> PolicySequence:
        self.check_t(t)
        if n == 0: return []
        self.check_n(n)

        ps_tail = self.randomPS(t + 1, n - 1)
        p = self.randomExt(t, ps_tail)
        return [p] + ps_tail

    # Generates a histogram of the binned empirical probability distribution of values 
    # of a policy of length 'n', measured from time step 't' and state 'x'.
    def valDistribution(self, t: int, n: int, x: State, n_points = 1000, n_bins = 'auto') -> None:
        self.check_t(t)
        self.check_n(n)
        self.check_x(t, x)

        data = list()
        for i in range(n_points):
            ps = self.randomPS(t, n)
            val = ps[0][x][1]
            data.append(val)

        counts, bin_edges = np.histogram(data, bins=n_bins, density=False)
        probabilities = counts / counts.sum()
        bin_centers = (bin_edges[:-1] + bin_edges[1:]) / 2

        plt.bar(bin_centers, probabilities, width=(bin_edges[1] - bin_edges[0]) * 0.9)
        plt.xlabel('Value')
        plt.ylabel('Probability')
        plt.title('Binned Empirical Probability Distribution')
        plt.grid(axis='y', c = "white")
        plt.show()
    
    # Given a time step 't', a time horizon 'n' and a state 'x', returns the values 
    # attained for choosing each valid action in this time step and state.
    def allActionVals(self, t: int, n: int, x: State) -> dict[Action, float]:
        self.check_t(t)
        self.check_n(n)
        self.check_x(t, x)

        vals = dict()
        ps_tail = self.bi(t+1, n-1)
        for action in self.actions(t, x):
            p = {x: (action, None)}
            value = self.val(t, [p] + ps_tail, x)
            vals[action] = value
        return vals
    
    # Generates a plot that for each time horizon from 1 to 'n', displays the values of all 
    # valid actions in time step 't' and state 'x'. 
    def plotActionsToHorizon(self, t: int, n: int, x: State) -> None:
        toPlot = dict()
        actions = self.actions(t, x)
        for action in actions:
            toPlot[action] = []
        for i in range(1, n+1):
            vals = self.allActionVals(t, i, x)
            for action, val in vals.items():
                toPlot[action].append(val)
        for action, values in toPlot.items():
            plt.plot(range(n), values, label=str(action))

        plt.xlabel("Time horizon")
        plt.ylabel("Value")
        plt.title("Action values over time horizon")
        plt.legend()
        plt.grid(True)
        plt.show()