import random
from enum import Enum, auto
from typing import TypeAlias

import matplotlib.pyplot as plt
from src.application.theoryMemoization import SDP

"""
Parent-class that takes as input one "Parent" SDP, which should not have a
reward function defined, and arbitrarily many "Children" that should inherit 
from "Parent", but have their own separate reward-functions. The valueCloud
function only works with two children (if more are defined, function will use
the first two).
"""

class State(Enum):
    pass

class Action(Enum):
    pass

Policy: TypeAlias = dict[State, tuple[Action, float]]
PolicySequence: TypeAlias = list[dict[State, tuple[Action, float|None]]]

class SDP_Pareto():
    def __init__(self, Parent: SDP, Children: list[SDP]):
        self.parent = Parent
        self.children = Children

    def randomExt(self, t: int, ps_tails: list[PolicySequence]) -> list[Policy]:
        # Each policy will be indentical in state-action pairings, but contain different values.
        policies = [dict() for i in range(len(self.children))]
        for state in self.parent.states(t):
            actions = self.parent.actions(t, state)
            random_action = random.choice(actions)
            p = {state: (random_action, None)}
            for i in range(len(self.children)):
                child = self.children[i]
                ps_tail = ps_tails[i]
                value = child.val(t, [p] + ps_tail, state)
                policies[i][state] = (random_action, value)
        return policies

    def randomPS(self, t: int, n: int) -> list[PolicySequence, PolicySequence]:
        if n == 0:
            return [list() for i in range(len(self.children))]
        else:
            ps_tails = self.randomPS(t + 1, n - 1)
            policies = self.randomExt(t, ps_tails)

            return [[policies[i]] + ps_tails[i] for i in range(len(self.children))]

    def valueCloud(self, t, n, x, n_points, pareto_front = True):
        x_axis = []
        y_axis = []
        for i in range(n_points):
            policies = self.randomPS(t, n)
            val_1 = policies[0][0][x][1]
            val_2 = policies[1][0][x][1]
            x_axis.append(val_1)
            y_axis.append(val_2)
        
        coords = sorted(list(zip(x_axis, y_axis)), key = lambda coord: coord[0])
        pareto_x = [min(x_axis)]
        pareto_y = [max(y_axis)]

        if pareto_front:
            for i in range(len(coords) -1):
                x, y = coords[i]
                if all(y_prim < y for x_prim, y_prim in coords[i+1:]):
                    pareto_x.append(x)
                    pareto_y.append(y)
            pareto_x.append(max(x_axis))
            pareto_y.append(min(y_axis))
            plt.plot(pareto_x, pareto_y)

        plt.scatter(x_axis, y_axis, c="blue", s=.8)
        plt.show()
        return x_axis, y_axis