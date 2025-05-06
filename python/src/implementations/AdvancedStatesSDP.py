from enum import Enum, auto
from typing import TypeAlias

import numpy as np
from scipy.stats import norm
from src.application.theoryMemorization import SDP

"""
Declare all states of the SDP below:
"""
State: TypeAlias = tuple # Needed because internal functions expect states to be of type 'State'
# def generate_states(t: int) -> int:
#     decisionValues = np.arange(0, 3, 1)
#     climateValues = np.arange(1, 6, 1)
#     econValues = np.arange(1, 6, 1)
#     return [(x1, x2, x3) for x1 in decisionValues for x2 in climateValues for x3 in econValues]
"""
Declare all actions of the SDP below:
"""
class Action(Enum):
    CLIM = 0
    PASSIVE = 1
    ECON = 2

"""
Define transition probabilities below:
"""
drifts = {
    Action.CLIM:      (1.0, -0.4),
    Action.PASSIVE:  (0.0, 0.0),
    Action.ECON:     (-0.4, 1.0)
}

class Specification(SDP):
    def __init__(self, decisionValues, climValues, econValues, sigma):
        self.decisionValues = decisionValues
        self.climValues = climValues
        self.econValues = econValues
        self.sigma = sigma

    # Returns the value considered as the 'baseline' of specification.
    @property
    def zero(self) -> float:
        return 0.0

    # Returns the discount rate for adding rewards from later time steps 
    # (1 means no discounting takes place).
    @property
    def discountRate(self) -> float:
        return 1.0
    
    def states(self, t: int) -> list[State]:
        return [(x1, x2, x3) for x1 in self.decisionValues for x2 in self.climValues for x3 in self.econValues]


    # Returns all actions 'y' that are valid in time step 't'
    # and state 'x'.
    def actions(self, t: int, x: State) -> list[Action] | list[None]:
        if x in self.states(t):
            return [Action.CLIM, Action.PASSIVE, Action.ECON]
        else:
            raise ValueError(f"Invalid State: '{x}' for time step '{t}'.")

    # Given a time step 't', a state 'x' and an action 'y', returns the
    # probability distribution over states to be entered in time step 't+1'.
    def nextFunc(self, t: int, x: State, y: State) -> dict[State, float]:
        dec, clim, econ = x
        newDec = y.value

        muClim = clim + drifts[y][0]
        muEcon = econ + drifts[y][1]

        climEdges = np.concatenate(([-np.inf],
                                (self.climValues[:-1] + self.climValues[1:]) / 2,
                                [np.inf]))        
        
        pmfClim = {}
        for k, lower, upper in zip(self.climValues, climEdges[:-1], climEdges[1:]):
            pClim = norm.cdf((upper - muClim) / self.sigma) - norm.cdf((lower - muClim) / self.sigma)
            pmfClim[k] = pClim
        
        econEdges = np.concatenate(([-np.inf],
                                (self.econValues[:-1] + self.econValues[1:]) / 2,
                                [np.inf]))
        
        pmfEcon = {}
        for k, lower, upper in zip(econValues, econEdges[:-1], econEdges[1:]):
            pEcon = norm.cdf((upper - muEcon) / self.sigma) - norm.cdf((lower - muEcon) / self.sigma)
            pmfEcon[k] = pEcon

        dist = list()
        for cVal, pClim in pmfClim.items():
            for eVal, pEcon in pmfEcon.items():
                dist.append(((newDec, cVal, eVal), float(pClim) * float(pEcon)))
        
        return self.mkSimpleProb(dist)


    # Given a time step 't', a state 'x' and an action 'y', returns
    # the reward of ending up in state 'next_x' in time step 't+1'.
    def reward(self, t: int, x: State, y: Action, next_x: State) -> float:
        return float(next_x[1]) / float(max(self.climValues)) * float(next_x[2]) / float(max(self.climValues)) # Add implementation here...


"""
Instantiate your specification to run its functions.
"""
decisionValues = np.arange(0, 3, 1)
climValues = np.arange(1, 6, 1)
econValues = np.arange(1, 6, 1)
SDP1 = Specification(decisionValues, climValues, econValues, 0.5)

# result = SDP1.states(0)
# result = SDP1.actions(0, (0, 1, 1))
# result = SDP1.nextFunc(0, (0, 1, 1), Action.ECON)
# result = SDP1.safe_reward(0, (0, 1, 1), Action.ECON, (2, 5, 5))
# result = SDP1.bi(0, 1)
result = SDP1.best(0, 1, (0, 1, 1))
# result = SDP1.worst(0, 1, (0, 1, 1))
# result = SDP1.mMeas(0, 7, (0, 1, 1))

print(result)