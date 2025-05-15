import sys, os
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..')))

import matplotlib.pyplot as plt
from typing import TypeAlias
from enum import Enum
import numpy as np
import random


"""
CURRENTLY NEEDS TO BE RUN IN CONNECTION TO REST OF FRAMEWORK, DISCUSS HOW AT GROUP MEETING.
Below are functions that can be used to produce plots of results from SDP:s.
"""

from src.implementations.MatterMostSDP import MatterMost as Implementation

class State(Enum):
    pass

class Action(Enum):
    pass

class SDP_Plots(Implementation):
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