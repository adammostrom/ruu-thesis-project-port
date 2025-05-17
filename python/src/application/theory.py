import random
from abc import ABC, abstractmethod
from enum import Enum
from typing import TypeAlias

import matplotlib.pyplot as plt  # TODO why import plotting in the theory file?
import numpy as np
from src.utils.errorChecks import ErrorChecks
from src.utils.mathOperations import MathOperations

"""
This file contains a python translation of the main functions neccessary to create
sequential decision problems (SDP:s) as described in the article "Responsibility Under
Uncertainty: Which Climate Decisions Matter Most?" by Botta et al.
"""

class State(Enum):
    pass

class Action(Enum):
    pass

Policy: TypeAlias = dict[State, Action]
PolicySequence: TypeAlias = list[Policy]

# Abstract Base Class (Enforcing required methods)
class SDP(ABC, ErrorChecks, MathOperations):

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

    # TODO This discount rate is reasonable to have, but not included as a separate concept in our SDP papers, but instead "baked into" the add method. Remove or explain the connection.
    # TODO The add method seems to not be explained. Perhaps it is explained elsewhere? (in MathOperations?) Do you mean that add(x,y) = x + discount*y (Aha - I found it in mathOperations.py)
    # TODO I think zero and add "belong together" - thus I suggest you move zero to MathOperations but leave a comment somewhere in here to mention them both (and perhaps discount).

    # Returns all states 'x' that are valid in time step 't'.
    @abstractmethod
    def states(self, t: int) -> list[State]:
        pass # Problem-specific, to be implemented by user in specification.

    # Returns all actions 'y' that are valid in time step 't' and state 'x'.
    @abstractmethod
    def actions(self, t: int, x: State) -> list[Action] | list[None]: # TODO why use list[None] as an error case? I think an empty list would be more natural if there are truly no actions available.
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
    # TODO why allow a list[None] instead of a policy sequence? What is that supposed to mean?
    def val(self, t: int, ps: PolicySequence | list[None], x: State) -> float:
        self.check_t(t)
        self.check_ps(ps)
        self.check_x(t, x)

        value = self.zero
        M_vals = list()
        if len(ps) == 0:
            return value

        # TODO why return zero here? I think raising a ValueError or similar would be better.
        if x not in ps[0]:
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
    # TODO why allow a list[None] instead of a policy sequence? What is that supposed to mean?
    def bestExt(self, t: int, ps_tail: PolicySequence | list[None]) -> Policy:
        self.check_t(t)
        self.check_ps_tail(ps_tail)  # TODO why a special "check" for a ps_tail?

        policy = dict()
        for state in self.states(t):
            best_value = -np.inf
            best_action = None
            for action in self.actions(t, state):
                # TODO Add a comment explaining that, although p is only a "partial" policy, val will still succeed because it is only called on state.
                p = {state: action}
                value = self.val(t, [p] + ps_tail, state)
                if value >= best_value:
                    best_value = value
                    best_action = action
            # TODO note that best_action may still be None in case the list of actions was empty (or raise ValueError or similar)
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

    # Given a time step 't' and a policy sequence 'ps_tail', returns
    # a random extension to this policy sequence.
    def randomExt(self, t: int, ps_tail: PolicySequence) -> Policy:
        self.check_t(t)
        self.check_ps_tail(ps_tail)

        policy = dict()
        for state in self.states(t):
            actions = self.actions(t, state)
            random_action = random.choice(actions)
            p = {state: random_action}
            # TODO why compute a value and then not use it?
            value = self.val(t, [p] + ps_tail, state)
            policy[state] = random_action
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

    # Given a time step 't' and a time horizon 'n', returns a random
    # policy sequence of length 'n' starting at time step 't'.
    def randomPS(self, t: int, n: int) -> PolicySequence:
        self.check_t(t)
        if n == 0: return []
        self.check_n(n)

        ps_tail = self.randomPS(t + 1, n - 1)
        p = self.randomExt(t, ps_tail)
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
            b = "No Action"   # TODO I think keeping None (or raising an exception) is better than inserting a "random string"
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
        b = p[x]
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
