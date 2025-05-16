from enum import Enum
from typing import TypeAlias

import numpy as np

from src.application.theoryMemorization import SDP

"""
The following file shows an implementation of the SDP-framework from "theoryMemorization.py" 
on a few simple labyrinth solving problems.

We start by defining a general class for labyrinth SDP:s, from which we can inherit
when creating specific labyrinths.
"""

State: TypeAlias = tuple[int, int] # Needed because internal functions expect states to be of type 'State'
def generate_states(map: np.ndarray) -> list[State]:
    states = []
    h, w = np.shape(map)
    for row in range(h):
        for col in range(w):
            if map[row,col] == 1:
                states.append((row+1, col+1))
    return states

class Action(Enum):
    Left = "Left"
    Right = "Right"
    Up = "Up"
    Down = "Down"
    Stay = "Stay"


class Labyrinth(SDP):
    def __init__(self):
        self._states = None # Implementation-specific, derived from self.map
        self.probs = None # Implementation-specific
        self.map = None # Implementation-specific
        self.height = np.shape(self.map)[0]
        self.width = np.shape(self.map)[1]

    @property
    def zero(self) -> float:
        return 0.0
    
    @property
    def discountRate(self) -> float:
        return 1.0
    
    def states(self, t: int) -> list[State]:
        if not self._states:
            self._states = generate_states(self.map)
        return self._states

    # Function that returns the possible actions in any allowed state.
    def actions(self, t: int, x: State) -> list[Action] | list[None]:
        actions = [Action.Stay]
        row = x[0]
        col = x[1]
        h, w = self.height, self.width
        adjacent = {
                "Left":  (row, col-1), 
                "Right": (row, col+1),
                "Up":    (row-1, col), 
                "Down":  (row+1, col)
                }
        for dir, coord in adjacent.items():
            if 1 <= coord[0] <= h and 1 <= coord[1] <= w and self.map[coord[0]-1, coord[1]-1]:
                actions.append(Action(dir))
        return actions

    # Made to work for labyrinths of any size.
    def nextFunc(self, t: int, x: State, y: Action) -> dict[State, float]:
        row = x[0]
        col = x[1]
        h, w = self.height, self.width
        viable = {}
        unviable = {}
        transitions = []
        actions = self.actions(t, x)
        adjacent = {
                "Left":  (row, col-1), 
                "Right": (row, col+1),
                "Up":    (row-1, col), 
                "Down":  (row+1, col)
                }
        for dir, coord in adjacent.items():
            if Action(dir) in actions:
                viable[dir] = coord
            else:
                unviable[dir] = coord
        for dir, coord in viable.items():
            transitions.append(((coord), self.probs[(dir, y)]))
        stay_prob = sum([self.probs[(d, y)] for d in unviable.keys()]) + self.probs["Stay", y]
        transitions.append((x, stay_prob))
        return self.mkSimpleProb(transitions)

    def reward(self, t: int, x: State, y: Action, x_prim: State) -> int:
        pass  # To be completed in specific implementations.

    # Function that visualizes the input policy on the maze.
    def mazeVis(self, dim: tuple[int, int],p: dict[State, tuple[Action, float]], n) -> str:
        padded_dim = (dim[0]+2, dim[1]+2)
        map = np.full(padded_dim, "■")
        symbols = {}
        for state, (action, _) in p.items():
            match action:
                case Action.Left:
                    symbols[state] = "←"
                case Action.Right:
                    symbols[state] = "→"
                case Action.Up:
                    symbols[state] = "↑"
                case Action.Down:
                    symbols[state] = "↓"
                case Action.Stay:
                    symbols[state] = "X"
                case _:
                    raise ValueError(f"Invalid state found: '{state}'")

            map[state] = symbols[state]
        print(f"Optimal movement map with time horizon {n}")
        for row in map:
            print(" ".join(row))

"""
We begin by implementing a small labyrinth in a 3x3 grid as an SDP. It has one state for each grid cell,
named "R_C" (R being the row number and C being the column number). The center cell of the labyrinth - "22" -
is a wall, meaning this is not an allowed state in the SDP. For this SDP, actions are deterministic, meaning
that all transition probabilities are equal to either zero or one (e.g. if you try to go "Left",
then you definatelly will).
"""

probs_deterministic = {
    ("Left", Action.Left): 1.0,
    ("Right", Action.Left): 0.0,
    ("Up", Action.Left): 0.0,
    ("Down", Action.Left): 0.0,
    ("Stay", Action.Left): 0.0,

    ("Left", Action.Right): 0.0,
    ("Right", Action.Right): 1.0,
    ("Up", Action.Right): 0.0,
    ("Down", Action.Right): 0.0,
    ("Stay", Action.Right): 0.0,

    ("Left", Action.Up): 0.0,
    ("Right", Action.Up): 0.0,
    ("Up", Action.Up): 1.0,
    ("Down", Action.Up): 0.0,
    ("Stay", Action.Up): 0.0,

    ("Left", Action.Down): 0.0,
    ("Right", Action.Down): 0.0,
    ("Up", Action.Down): 0.0,
    ("Down", Action.Down): 1.0,
    ("Stay", Action.Down): 0.0,

    ("Left", Action.Stay): 0.0,
    ("Right", Action.Stay): 0.0,
    ("Up", Action.Stay): 0.0,
    ("Down", Action.Stay): 0.0,
    ("Stay", Action.Stay): 1.0,
}

small_maze_map = np.array(
            [[1, 1, 1],
             [1, 0, 1],
             [1, 1, 1]]
            )


class SmallLabyrinthDet(Labyrinth):
    def __init__(self):
        self.map     = small_maze_map
        self.probs   = probs_deterministic
        self._states = None
        h, w = self.map.shape
        self.height, self.width = h, w

    def reward(self, t: int, x: State, y: Action, x_prim: State) -> int:
        match x_prim:
            case (3, 3):
                return 1.0
            case (3, 2):
                return -10.0
            case _:
                return -0.1

smallMazeDet = SmallLabyrinthDet()

"""
Below are some sanity checks to demonstrate this first maze implementation.
"""

# print("Best choises in small deterministic maze:")
# bests = []
# for i in range(1, 8):
#     bests.append(smallMazeDet.best(0, i, (1, 1)))

# for result in bests:
#     print(result)

# ps = smallMazeDet.bi(0, 6)
# n = 1
# for p in reversed(ps):
#     smallMazeDet.mazeVis((3, 3),p , n)
#     n += 1

"""
Next we modify the probabilities of states transitions. Maybe the labyrinth is dark, 
or perhaps our maze-solver has had too much to drink? Now, there is a five percent chance of
the decision maker staying instead of taking a step, and of choosing a direction that is perpendicular
to the intended direction (the completely opposite direction still has a probability of zero).
When trying to stand still, there is a five percent chance of moving in each direction.
"""

probs_probabilistic = {
    ("Left", Action.Left):  0.85,
    ("Right", Action.Left):  0.0,
    ("Up", Action.Left):  0.05,
    ("Down", Action.Left):  0.05,
    ("Stay", Action.Left):  0.05,

    ("Left", Action.Right): 0.0,
    ("Right", Action.Right): 0.85,
    ("Up", Action.Right): 0.05,
    ("Down", Action.Right): 0.05,
    ("Stay", Action.Right): 0.05,

    ("Left", Action.Up):    0.05,
    ("Right", Action.Up):    0.05,
    ("Up", Action.Up):    0.85,
    ("Down", Action.Up):    0.0,
    ("Stay", Action.Up):    0.05,

    ("Left", Action.Down):  0.05,
    ("Right", Action.Down):  0.05,
    ("Up", Action.Down):  0.0,
    ("Down", Action.Down):  0.85,
    ("Stay", Action.Down):  0.05,

    ("Left", Action.Stay):  0.05,
    ("Right", Action.Stay):  0.05,
    ("Up", Action.Stay):  0.05,
    ("Down", Action.Stay):  0.05,
    ("Stay", Action.Stay):  0.80,
}

class SmallLabyrinthProb(Labyrinth):
    def __init__(self):
        self.map = small_maze_map
        self.probs = probs_probabilistic
        self._states = None
        h, w = self.map.shape
        self.height, self.width = h, w

    def reward(self, t: int, x: State, y: Action, x_prim: State) -> int:
        match x_prim:
            case (3, 3):
                return 1.0
            case (3, 2):
                return -10.0
            case _:
                return -0.1

smallMazeProb = SmallLabyrinthProb()

"""
Once again, we could run are some sanity checks for this SDP:
"""

# print("Best choises in small probabilistic maze:")
# bests = []
# for i in range(1, 8):
#     bests.append(smallMazeProb.best(0, i, (1, 1)))

# for result in bests:
#     print(result)

# ps = smallMazeProb.bi(0, 6)
# n = 1
# for p in reversed(ps):
#     smallMazeProb.mazeVis((3, 3),p , n)
#     n += 1

"""
Next we try out a slightly larger labyrinth. We create a new class that once again inherits from the general 
labyrinth class. Actions remain the same as before, and we use the probabilistic transition probabilities
from earlier so these do not need to be redefined.
"""

medium_maze_map = np.array(
            [[1, 0, 1, 1, 1],
            [1, 1, 1, 0, 1],
            [1, 0, 1, 1, 1],
            [1, 1, 1, 0, 1]]
            )

class MediumLabyrinth(Labyrinth):
    def __init__(self):
        self.map = medium_maze_map
        self.probs = probs_probabilistic
        self._states = None
        h, w = self.map.shape
        self.height, self.width = h, w

    def reward(self, t: int, x: State, y: Action, x_prim: State) -> int:
        match x_prim:
            case (4, 5):
                return 1.0
            case (2, 2) | (3, 4):
                return -20.0
            case _:
                return -0.1

mediumMaze = MediumLabyrinth()

"""
Some sanity checks for this SDP:
"""
# print("Best choises in medium sized probabilistic maze:")
# bests = []
# for i in range(1, 8):
#     bests.append(mediumMaze.best(0, i, (1, 1)))

# for result in bests:
#     print(result)

# n = 100
# print(mediumMaze.best(0, n, (1, 1)))
# ps = mediumMaze.bi(0, n)
# mediumMaze.mazeVis((4, 5),ps[0] , n)

"""
Finally, we try out an even larger labyrinth. As can be seen in the reward function below,
there are more "traps" in this one. When tried out with the probabilistic transition
probabilities, these traps resulted in the optimal solution often being simply to get as
far away from the traps as possible, not getting to the exit of the maze. Therefore, the
deterministic transition probabilities were used when running sanity checks on this labyrinth.
"""

large_maze_map = np.array(
            [[1, 1, 1, 0, 1, 1, 1, 1],
             [1, 0, 1, 1, 1, 0, 0, 1],
             [1, 0, 1, 0, 1, 0, 0, 1],
             [1, 1, 1, 0, 1, 1, 1, 1],
             [0, 1, 0, 0, 0, 1, 0, 1],
             [1, 1, 1, 1, 1, 1, 0, 1],
             [1, 0, 0, 1, 0, 1, 0, 1],
             [1, 1, 1, 1, 0, 1, 1, 1]]
            )

class LargestLabyrinth(Labyrinth):
    def __init__(self):
        self.map = large_maze_map
        self.probs = probs_deterministic
        self._states = None
        h, w = self.map.shape
        self.height, self.width = h, w

    def reward(self, t: int, x: State, y: Action, x_prim: State) -> int:
        match x_prim:
            case (1, 8):
                return 10.0
            case (2, 4) | (2, 8) | (4, 3) | (5, 6) | (6, 3):
                return -1000.0
            case _:
                return -0.1

largeMaze = LargestLabyrinth()

"""
Just as before, we could run some sanity checks:
"""
# print("Best choises in largest maze with deterministic transition probs:")
# bests = []
# for i in range(1, 8):
#     bests.append(largeMaze.best(0, i, (1, 1)))

# for result in bests:
#     print(result)

# n = 100
# print(largeMaze.best(0, n, (1, 1)))
# ps = largeMaze.bi(0, n)
# largeMaze.mazeVis((8, 8),ps[0] , n)