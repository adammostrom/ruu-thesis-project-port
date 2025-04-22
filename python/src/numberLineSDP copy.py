from enum import Enum, auto

from theory import SDP

# Declare all states of the SDP below:

class State(Enum):
    pass

def generate_states(n: int) -> State:
    # Generate states
    members = {}
    for i in range(-n, n + 1):
        if i < 0:
            name = f"NEG{i*-1}"
        elif i == 0:
            name = "ZERO"
        else:
            name = f"POS{i}"
        members[name] = i

    return Enum('State', members)

# Declare all actions of the SDP below:
class Action(Enum):
    Left = auto()
    Right = auto()
    Stay = auto()

# Define transition probabilities below:
pL_Left = 0.85
pS_Left = 0.1
pR_Left = 0.05

pL_Stay = 0.1
pS_Stay = 0.8
pR_Stay = 0.1

pL_Right = 0.05
pS_Right = 0.1
pR_Right = 0.85

class Specification(SDP):

    # Returns the value considered as the 'baseline' of specification.
    @property
    def zero(self) -> float:
        return 0.0
    
    # Returns all states 'x' that are valid in time step 't'.
    def states(self, t: int) -> list[State]:
        return generate_states(t)

    # Returns all actions 'y' that are valid in time step 't'
    # and state 'x'.
    def actions(self, t: int, x: State) -> list[str] | list[None]:
        if x in self.states(t):
            return [Action.Left, Action.Right, Action.Stay]
        else:
            raise ValueError(f"Invalid State: '{x}'.")

    # Given a time step 't', a state 'x' and an action 'y', returns the
    # probability distribution over states to be entered in time step 't+1'.
    def nextFunc(self, t: int, x: State, y: State) -> dict[State, float]:
        pass

    # Given a time step 't', a state 'x' and an action 'y', returns
    # the reward of ending up in state 'next_x' in time step 't+1'.
    def reward(self, t: int, x: State, y: Action, next_x: State) -> int:
        pass

SDP1 = Specification()



# Function that returns the possible actions in any allowed state.

# For a current time step, state and action, returns the probabilities of entering each state in the next time step.
def nextFunc(t: int, x: str, y: str) -> dict[str, float]:
    # Actions for all states are "Left", "Stay", or "Right".
        if x == "-2":
            if y == "Left":
                return mkSimpleProb(
                    [
                        ("-2", pL_Left + pS_Left),
                        ("-1", pR_Left)
                    ]
                )
            elif y == "Stay":
                return mkSimpleProb(
                    [
                        ("-2", pL_Stay + pS_Stay),
                        ("-1", pR_Stay)
                    ]
                )
            elif y == "Right":
                 return mkSimpleProb(
                      [
                           ("-2", pL_Right + pS_Right),
                           ("-1", pR_Right)
                      ]
                 )
            else:
                raise ValueError("Invalid control for state=-2.")
        elif x == "-1":
            if y == "Left":
                return mkSimpleProb(
                    [
                        ("-2", pL_Left),
                        ("-1", pS_Left),
                        ("0", pR_Left)
                    ]
                )
            elif y == "Stay":
                return mkSimpleProb(
                    [
                        ("-2", pL_Stay),
                        ("-1", pS_Stay),
                        ("0", pR_Stay)
                    ]
                )
            elif y == "Right":
                 return mkSimpleProb(
                      [
                        ("-2", pL_Right),
                        ("-1", pS_Right),
                        ("0", pR_Right)
                      ]
                 )
            else:
                raise ValueError("Invalid control for state=-1.")
        elif x == "0":
            if y == "Left":
                return mkSimpleProb(
                    [
                        ("-1", pL_Left),
                        ("0", pS_Left),
                        ("1", pR_Left)
                    ]
                )
            elif y == "Stay":
                return mkSimpleProb(
                    [
                        ("-1", pL_Stay),
                        ("0", pS_Stay),
                        ("1", pR_Stay)
                    ]
                )
            elif y == "Right":
                 return mkSimpleProb(
                      [
                        ("-1", pL_Right),
                        ("0", pS_Right),
                        ("1", pR_Right)
                      ]
                 )
            else:
                raise ValueError("Invalid control for state=0.")
        elif x == "1":
            if y == "Left":
                return mkSimpleProb(
                    [
                        ("0", pL_Left),
                        ("1", pS_Left),
                        ("2", pR_Left)
                    ]
                )
            elif y == "Stay":
                return mkSimpleProb(
                    [
                        ("0", pL_Stay),
                        ("1", pS_Stay),
                        ("2", pR_Stay)
                    ]
                )
            elif y == "Right":
                 return mkSimpleProb(
                      [
                        ("0", pL_Right),
                        ("1", pS_Right),
                        ("2", pR_Right)
                      ]
                 )
            else:
                raise ValueError("Invalid control for state=1.")
        elif x == "2":
            if y == "Left":
                return mkSimpleProb(
                    [
                        ("1", pL_Left),
                        ("2", pS_Left + pR_Left),
                    ]
                )
            elif y == "Stay":
                return mkSimpleProb(
                    [
                        ("1", pL_Stay),
                        ("2", pS_Stay + pR_Stay)
                    ]
                )
            elif y == "Right":
                 return mkSimpleProb(
                      [
                        ("1", pL_Right),
                        ("2", pS_Right + pR_Right)
                      ]
                 )
            else:
                raise ValueError("Invalid control for state=2.")
        else:
            raise ValueError(f"Invalid state: {x}")
      
# Reward function.
def reward(t: int, x: str, y: str, next_x: str) -> float:
    # Value is added for transitioning into states which do not have low economic
    # output and at the same time are not comitted to severe future climate change.
    if t < 0 or type(t) != int:
        raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")
    if x not in states:
        raise ValueError(f"Invalid state: '{x}'")
    if y not in actions(x):
        raise ValueError(f"Invalid action: '{y}'")
    if next_x not in states:
        raise ValueError(f"Invalid next state: '{next_x}'")
    return 1.0 if next_x == "2" else 0.0

# Function defining how to add rewards together.
def add(a: float, b: float) -> float:
    if type(a) != float or type(b) != float:
        raise TypeError(f"Inputs must be of type 'float', not '{type(a).__name__}' and '{type(b).__name__}'.")
    return a + b # In default implementation, returns regular floating point addition.

# Function for measuring a certain value.
def meas(val: float, pr: float) -> float:
    if type(val) != float or type(pr) != float:
        raise TypeError(f"Inputs must be of type 'float', not '{type(val).__name__}' and '{type(pr).__name__}'.")
    return val * pr # In default implementation, returns the expected value.

# Default value of zero-length policy sequences.
zero = 0.0
# Computing the total expected value from a policy sequence when starting at time t in state x.
def val(t: int, ps: list[dict[str, str]], x: str) -> float:
    if t < 0 or type(t) != int:
        raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")
    if type(ps) != list:
        raise TypeError(f"Invalid policy list, must be list of dictionaries (or empty list).")
    if x not in states:
        raise ValueError(f"Invalid state: '{x}'")
    value = zero
    if len(ps) == 0:
        return value
    y = ps[0][x]
    m_next = nextFunc(t, x, y)
    for x_prim, pr in m_next.items():
        value += meas(add(reward(t, x, y, x_prim), val(t+1, ps[1:], x_prim)), pr)
    return value

# Computes the best single policy to add to an existing policy sequence.
def bestExt(t: int, ps_tail: list[dict[str, str]]) -> dict[str, str]:
    policy = dict()

    for state in states:
        best_value = -np.inf
        best_action = None

        for action in actions(state):
            # Calculate value of taking action in state
            p = {state: action}
            value = val(t, [p] + ps_tail, state)
            # Choose the action with the highest expected value
            if value >= best_value:
                best_value = value
                best_action = action

        policy[state] = best_action

    return policy

def worstExt(t: int, ps_tail: list[dict[str, str]] | list[None]) -> dict[str, str]:
    if t < 0 or type(t) != int:
        raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")
    if type(ps_tail) != list:
        raise TypeError(f"Invalid ps_tail, must be list of dictionaries (or empty list).")
    
    policy = dict()

    for state in states:
        worst_value = np.inf
        worst_action = None

        # For each available action in the current state
        for action in actions(state):
            # Calculate value of taking action in state
            p = {state: action}
            value = val(t, [p] + ps_tail, state)
            # Choose the action with the highest expected value
            if value <= worst_value:
                worst_value = value
                worst_action = action

        policy[state] = worst_action

    return policy

# Builds an optimal policy sequence by recursively adding the best extension (starting from the end).
def bi(t: int, n: int) -> list[dict[str, str]]:
    if n == 0:
        return []
    else:
        ps_tail = bi(t + 1, n - 1)
        p = bestExt(t, ps_tail)
        return [p] + ps_tail
    
# For a given time step, state and decision horizon, returns the optimal action and the
# expected value of the sequence it starts (assuming the rest of the sequence is optimal).
def best(t: int, n: int, x: str) -> str:
    if n <= 0:
        raise ValueError("The horizon must be greater than zero!")
    ps = bi(t + 1, n - 1)
    p = bestExt(t, ps)
    b = p[x]
    vb = val(t, [p] + ps, x)
    return f"Horizon, best, value : {n}, {b}, {vb}"

# Computing the best decision for different decision horizons.
"""
bests = []
for i in range(1, 3):
    bests.append(best(0, i, "2"))

for b in bests:
    print(b)
"""

# Returns a value between 0 and 1, where 0 means "does not matter at all"
# and 1 means "matters maximally" to achieving the defined goal of the SDP.
def mMeas(t: int, n: int, x: str) -> float:
    if x in ["SHU", "SHC", "SLU", "SLC"]:
        return 0
    else:
        ps = bi(t, n)
        ps_prim = copy.deepcopy(ps)
        if ps[0][x] == "Start":
            ps_prim[0][x] = "Delay"
        else:
            ps_prim[0][x] = "Start"

        best_action_val = val(t, ps, x)
        worst_action_val = val(t, ps_prim, x)

        return (best_action_val - worst_action_val) / best_action_val