from enum import Enum, auto

import numpy as np
from theoryMemorization import SDP

"""
The following file shows an implementation of the SDP-framework from "theory.py" on a few simple
labyrinths.

We start by defining a general class for labyrinth SDP:s, from which we can inherit
when creating specific labyrinths.
"""

class State(Enum):
    pass # To be completed in specific implementations.

class Action(Enum):
    pass # To be completed in specific implementations.


class Labyrinth(SDP):
    def __init__(self, states, actions, probs, maze_map):
        self._states = states
        self._action = actions
        self._probs = probs
        self._maze_map = maze_map

    # Map that defines which cells are traversable (paths) and which are not (walls).
    @property
    def maze_map(self) -> list:
        return self._maze_map

    @property
    def probs(self) -> dict:
        return self._probs
    
    @property
    def states(self) -> list[State]:
        return self._states

    # Function that returns the possible actions in any allowed state.
    def actions(self, t: int, x: State) -> list[str] | list[None]:
        actions = []
        row = x.value[0]
        col = x.value[1]
        maze_dims = self.maze_map.shape
        for dir, coord in {"Left": (row, col-1), "Right": (row, col+1),
                            "Up": (row-1, col), "Down": (row+1, col), "Stay": (row, col)}.items():
            if 1 <= coord[0] <= maze_dims[0] and 1 <= coord[1] <= maze_dims[1] and self.maze_map[coord[0]-1, coord[1]-1]:
                actions.append(Action(dir))
        return actions

    # Made to work for labyrinths of any size.
    def nextFunc(self, t: int, x: State, y: State) -> dict[State, float]:
        if x not in self.states or y not in self.actions(t, x):
                raise ValueError(f"Invalid State and/or action: '{x}' '{y}'.")
        return self.mkSimpleProb(self.next_helper(t, x, y))

    def next_helper(self, t: int, x: State, y: Action) -> list[tuple[State, float]]:
        row = x.value[0]
        col = x.value[1]
        maze_dims = self.maze_map.shape
        viable = {}
        unviable = {}
        transitions = []
        for dir, coord in {"L": (row, col-1), "R": (row, col+1), "U": (row-1, col), "D": (row+1, col)}.items():
            if 1 <= coord[0] <= maze_dims[0] and 1 <= coord[1] <= maze_dims[1] and self.maze_map[coord[0]-1, coord[1]-1]:
                viable[dir] = coord
            else:
                unviable[dir] = coord
        for dir, coord in viable.items():
            transitions.append((State(coord), self._probs[(dir, y)]))
        stay_prob = sum([self._probs[(d, y)] for d in unviable.keys()]) + self._probs["S", y]
        transitions.append((x, stay_prob))
        return transitions

    def reward(self, t: int, x: State, y: Action, next_x: State) -> int:
        pass  # To be completed in specific implementations.

    # Function that visualizes the input policy on the maze.
    def mazeVis(self, dim: tuple[int, int],p: dict[State, Action], n) -> str:
        padded_dim = (dim[0]+2, dim[1]+2)
        map = np.full(padded_dim, "■")
        symbols = {}
        for state, action_pair in p.items():
            action = action_pair[0]
            index = (int(state.name[1]), int(state.name[2]))
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

            map[index] = symbols[state]
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

class State(Enum):
    _11 = (1, 1)
    _12 = (1, 2)
    _13 = (1, 3)
    _21 = (2, 1)
    _23 = (2, 3) 
    _31 = (3, 1)
    _32 = (3, 2)
    _33 = (3, 3)

states = list(State)

class Action(Enum):
    Left = "Left"
    Right = "Right"
    Up = "Up"
    Down = "Down"
    Stay = "Stay"

actions = list(Action)

probs_deterministic = {
    ("L", Action.Left): 1.0,
    ("R", Action.Left): 0.0,
    ("U", Action.Left): 0.0,
    ("D", Action.Left): 0.0,
    ("S", Action.Left): 0.0,

    ("L", Action.Right): 0.0,
    ("R", Action.Right): 1.0,
    ("U", Action.Right): 0.0,
    ("D", Action.Right): 0.0,
    ("S", Action.Right): 0.0,

    ("L", Action.Up): 0.0,
    ("R", Action.Up): 0.0,
    ("U", Action.Up): 1.0,
    ("D", Action.Up): 0.0,
    ("S", Action.Up): 0.0,

    ("L", Action.Down): 0.0,
    ("R", Action.Down): 0.0,
    ("U", Action.Down): 0.0,
    ("D", Action.Down): 1.0,
    ("S", Action.Down): 0.0,

    ("L", Action.Stay): 0.0,
    ("R", Action.Stay): 0.0,
    ("U", Action.Stay): 0.0,
    ("D", Action.Stay): 0.0,
    ("S", Action.Stay): 1.0,
}

smaller_maze_map = np.array(
            [[1, 1, 1],
             [1, 0, 1],
             [1, 1, 1]]
            )

class SmallLabyrinth(Labyrinth):
    def reward(self, t: int, x: State, y: Action, next_x: State) -> int:
        match next_x:
            case State._33:
                return 1.0
            case State._32:
                return -10.0
            case _:
                return -0.1

smallMazeDet = SmallLabyrinth(states, actions, probs_deterministic, smaller_maze_map)

"""
Below are some sanity checks for this first maze implementation.
"""

print("Best choises in small deterministic maze:")
bests = []
for i in range(1, 8):
    bests.append(smallMazeDet.best(0, i, State._11))

for result in bests:
    print(result)

ps = smallMazeDet.bi(0, 6)
n = 1
for p in reversed(ps):
    smallMazeDet.mazeVis((3, 3),p , n)
    n += 1

"""
Next we modify the probabilities of states transitions. Maybe the labyrinth is dark, 
or perhaps our maze-solver has had too much to drink? Now, there is a five percent chance of
the decision maker staying instead of taking a step, and of choosing a direction that is perpendicular
to the intended direction (the completely opposite direction still has a probability of zero).
When trying to stand still, there is a five percent chance of moving in each direction.
"""

probs_probabilistic = {
    ("L", Action.Left):  0.85,
    ("R", Action.Left):  0.0,
    ("U", Action.Left):  0.05,
    ("D", Action.Left):  0.05,
    ("S", Action.Left):  0.05,

    ("L", Action.Right): 0.0,
    ("R", Action.Right): 0.85,
    ("U", Action.Right): 0.05,
    ("D", Action.Right): 0.05,
    ("S", Action.Right): 0.05,

    ("L", Action.Up):    0.05,
    ("R", Action.Up):    0.05,
    ("U", Action.Up):    0.85,
    ("D", Action.Up):    0.0,
    ("S", Action.Up):    0.05,

    ("L", Action.Down):  0.05,
    ("R", Action.Down):  0.05,
    ("U", Action.Down):  0.0,
    ("D", Action.Down):  0.85,
    ("S", Action.Down):  0.05,

    ("L", Action.Stay):  0.05,
    ("R", Action.Stay):  0.05,
    ("U", Action.Stay):  0.05,
    ("D", Action.Stay):  0.05,
    ("S", Action.Stay):  0.80,
}

smallMazeProb = SmallLabyrinth(states, actions, probs_probabilistic, smaller_maze_map)

"""
Once again, we run are some sanity checks for this SDP:
"""

print("Best choises in small probabilistic maze:")
bests = []
for i in range(1, 8):
    bests.append(smallMazeProb.best(0, i, State._11))

for result in bests:
    print(result)

ps = smallMazeProb.bi(0, 6)
n = 1
for p in reversed(ps):
    smallMazeProb.mazeVis((3, 3),p , n)
    n += 1

"""
Finally, we try out a slightly larger labyrinth. As a larger maze needs a larger set of states, we redefine
our states, define a new maze map and then create a new class that once again inherits from the general 
labyrinth class. Actions remain the same as before, and we use the probabilistic transition probabilities
from earlier so these do not need to be redefined.
"""

class State(Enum):
    _11 = (1, 1)
    _13 = (1, 3)
    _14 = (1, 4)
    _15 = (1, 5)
    _21 = (2, 1)
    _22 = (2, 2)
    _23 = (2, 3)
    _25 = (2, 5)
    _31 = (3, 1)
    _33 = (3, 3)
    _34 = (3, 4)
    _35 = (3, 5)
    _41 = (4, 1)
    _42 = (4, 2)
    _43 = (4, 3)
    _45 = (4, 5)

states = list(State)

larger_maze_map = np.array(
            [[1, 0, 1, 1, 1],
            [1, 1, 1, 0, 1],
            [1, 0, 1, 1, 1],
            [1, 1, 1, 0, 1]]
            )

class LargerLabyrinth(Labyrinth):
    def reward(self, t: int, x: State, y: Action, next_x: State) -> int:
        match next_x:
            case State._45:
                return 1.0
            case State._22 | State._34:
                return -5.0
            case _:
                return -0.1

largerMaze = LargerLabyrinth(states, actions, probs_probabilistic, larger_maze_map)

"""
Just as before, we run some sanity for this SDP:
"""
print("Best choises in larger probabilistic maze:")
bests = []
for i in range(1, 8):
    bests.append(largerMaze.best(0, i, State._11))

for result in bests:
    print(result)

n = 100
print(largerMaze.best(0, n, State._11))
ps = largerMaze.bi(0, n)
largerMaze.mazeVis((4, 5),ps[0] , n)