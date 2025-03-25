import copy
import numpy as np

# Set of the allowed states in the SDP.
states = ["DHU", "DHC", "DLU", "DLC", "SHU", "SHC", "SLU", "SLC"]

# Function that returns the possible actions in any allowed state.
def actions(x: str) -> list[str] | list[None]:
    if x in ["DHU", "DHC", "DLU", "DLC"]:
        return ["Start", "Delay"]
    elif x in ["SHU", "SHC", "SLU", "SLC"]:
        return [None]
    else:
        raise ValueError(f"Invalid State: '{x}'.")

# Probabilities for the transition function.
pS_Start = 0.9
pD_Start = 1.0 - pS_Start

pD_Delay = 0.9
pS_Delay = 1.0 - pD_Delay

pL_S_DH = 0.7
pL_S_DL = 0.9
pL_S_SH = 0.3
pL_S_SL = 0.7

pH_S_DH = 1.0 - pL_S_DH
pH_S_DL = 1.0 - pL_S_DL
pH_S_SH = 1.0 - pL_S_SH
pH_S_SL = 1.0 - pL_S_SL

pL_D_DH = pL_S_SH
pL_D_DL = pL_S_SL
pH_D_DH = 1.0 - pL_D_DH
pH_D_DL = 1.0 - pL_D_DL

pU_S_0 = 0.9
pU_D_0 = 0.7
pC_S_0 = 1.0 - pU_S_0
pC_D_0 = 1.0 - pU_D_0

pU_S = 0.9
pU_D = 0.3
pC_S = 1.0 - pU_S
pC_D = 1.0 - pU_D

# Function that checks that no probabilities are negative, and then
# returns probabilities for entering all states in next time step.
def mkSimpleProb(pairs: list[tuple[str, float]]) -> dict[str, float]:
    dist: dict[str, float] = {}
    for st, pr in pairs:
        if pr >= 0:
            dist[st] = pr
    return dist

# For a current time step, state and action, returns the probabilities of entering each state in the next time step.
def nextFunc(t: int, x: str, y: str) -> dict[str, float]:
    # "y" can be "Start", "Delay", or None for S-states.
    if t < 0 or type(t) != int:
        raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")    
    if t == 0:
        # CASE: t == 0
        if x == "DHU":
            if y == "Start":
                return mkSimpleProb(
                    [
                        ("DHU", pD_Start * pH_D_DH * pU_D_0),
                        ("DHC", pD_Start * pH_D_DH * pC_D_0),
                        ("DLU", pD_Start * pL_D_DH * pU_D_0),
                        ("DLC", pD_Start * pL_D_DH * pC_D_0),
                        ("SHU", pS_Start * pH_S_DH * pU_S_0),
                        ("SHC", pS_Start * pH_S_DH * pC_S_0),
                        ("SLU", pS_Start * pL_S_DH * pU_S_0),
                        ("SLC", pS_Start * pL_S_DH * pC_S_0),
                    ]
                )
            elif y == "Delay":
                return mkSimpleProb(
                    [
                        ("DHU", pD_Delay * pH_D_DH * pU_D_0),
                        ("DHC", pD_Delay * pH_D_DH * pC_D_0),
                        ("DLU", pD_Delay * pL_D_DH * pU_D_0),
                        ("DLC", pD_Delay * pL_D_DH * pC_D_0),
                        ("SHU", pS_Delay * pH_S_DH * pU_S_0),
                        ("SHC", pS_Delay * pH_S_DH * pC_S_0),
                        ("SLU", pS_Delay * pL_S_DH * pU_S_0),
                        ("SLC", pS_Delay * pL_S_DH * pC_S_0),
                    ]
                )
            else:
                raise ValueError("Invalid control for DHU at t=0.")

        elif x == "DHC":
            if y == "Start":
                return mkSimpleProb(
                    [
                        ("DHC", pD_Start * pH_D_DH),
                        ("DLC", pD_Start * pL_D_DH),
                        ("SHC", pS_Start * pH_S_DH),
                        ("SLC", pS_Start * pL_S_DH),
                    ]
                )
            elif y == "Delay":
                return mkSimpleProb(
                    [
                        ("DHC", pD_Delay * pH_D_DH),
                        ("DLC", pD_Delay * pL_D_DH),
                        ("SHC", pS_Delay * pH_S_DH),
                        ("SLC", pS_Delay * pL_S_DH),
                    ]
                )
            else:
                raise ValueError("Invalid control for DHC at t=0.")

        elif x == "DLU":
            if y == "Start":
                return mkSimpleProb(
                    [
                        ("DHU", pD_Start * pH_D_DL * pU_D_0),
                        ("DHC", pD_Start * pH_D_DL * pC_D_0),
                        ("DLU", pD_Start * pL_D_DL * pU_D_0),
                        ("DLC", pD_Start * pL_D_DL * pC_D_0),
                        ("SHU", pS_Start * pH_S_DL * pU_S_0),
                        ("SHC", pS_Start * pH_S_DL * pC_S_0),
                        ("SLU", pS_Start * pL_S_DL * pU_S_0),
                        ("SLC", pS_Start * pL_S_DL * pC_S_0),
                    ]
                )
            elif y == "Delay":
                return mkSimpleProb(
                    [
                        ("DHU", pD_Delay * pH_D_DL * pU_D_0),
                        ("DHC", pD_Delay * pH_D_DL * pC_D_0),
                        ("DLU", pD_Delay * pL_D_DL * pU_D_0),
                        ("DLC", pD_Delay * pL_D_DL * pC_D_0),
                        ("SHU", pS_Delay * pH_S_DL * pU_S_0),
                        ("SHC", pS_Delay * pH_S_DL * pC_S_0),
                        ("SLU", pS_Delay * pL_S_DL * pU_S_0),
                        ("SLC", pS_Delay * pL_S_DL * pC_S_0),
                    ]
                )
            else:
                raise ValueError("Invalid control for DLU at t=0.")

        elif x == "DLC":
            if y == "Start":
                return mkSimpleProb(
                    [
                        ("DHC", pD_Start * pH_D_DL),
                        ("DLC", pD_Start * pL_D_DL),
                        ("SHC", pS_Start * pH_S_DL),
                        ("SLC", pS_Start * pL_S_DL),
                    ]
                )
            elif y == "Delay":
                return mkSimpleProb(
                    [
                        ("DHC", pD_Delay * pH_D_DL),
                        ("DLC", pD_Delay * pL_D_DL),
                        ("SHC", pS_Delay * pH_S_DL),
                        ("SLC", pS_Delay * pL_S_DL),
                    ]
                )
            else:
                raise ValueError("Invalid control for DLC at t=0.")

        elif x == "SHU":
            # In Idris: Theory.next Z SHU () = mkSimpleProb [...]
            # The control is '()', i.e. a unit (we represent it as None in Python).
            if y is None:
                return mkSimpleProb(
                    [
                        ("SHU", pH_S_SH * pU_S_0),
                        ("SHC", pH_S_SH * pC_S_0),
                        ("SLU", pL_S_SH * pU_S_0),
                        ("SLC", pL_S_SH * pC_S_0),
                    ]
                )
            else:
                raise ValueError("Invalid control for SHU at t=0 (should be None).")

        elif x == "SHC":
            if y is None:
                return mkSimpleProb(
                    [
                        ("SHC", pH_S_SH),
                        ("SLC", pL_S_SH),
                    ]
                )
            else:
                raise ValueError("Invalid control for SHC at t=0.")

        elif x == "SLU":
            if y is None:
                return mkSimpleProb(
                    [
                        ("SHU", pH_S_SL * pU_S_0),
                        ("SHC", pH_S_SL * pC_S_0),
                        ("SLU", pL_S_SL * pU_S_0),
                        ("SLC", pL_S_SL * pC_S_0),
                    ]
                )
            else:
                raise ValueError("Invalid control for SLU at t=0.")

        elif x == "SLC":
            if y is None:
                return mkSimpleProb(
                    [
                        ("SHC", pH_S_SL),
                        ("SLC", pL_S_SL),
                    ]
                )
            else:
                raise ValueError("Invalid control for SLC at t=0.")

        else:
            raise ValueError("Unexpected state at t=0.")

    else:
        # CASE: t > 0
        # The code is perfectly analogous but uses pU_S, pC_S, pU_D, pC_D
        # instead of pU_S_0, pC_S_0, pU_D_0, pC_D_0.

        if x == "DHU":
            if y == "Start":
                return mkSimpleProb(
                    [
                        ("DHU", pD_Start * pH_D_DH * pU_D),
                        ("DHC", pD_Start * pH_D_DH * pC_D),
                        ("DLU", pD_Start * pL_D_DH * pU_D),
                        ("DLC", pD_Start * pL_D_DH * pC_D),
                        ("SHU", pS_Start * pH_S_DH * pU_S),
                        ("SHC", pS_Start * pH_S_DH * pC_S),
                        ("SLU", pS_Start * pL_S_DH * pU_S),
                        ("SLC", pS_Start * pL_S_DH * pC_S),
                    ]
                )
            elif y == "Delay":
                return mkSimpleProb(
                    [
                        ("DHU", pD_Delay * pH_D_DH * pU_D),
                        ("DHC", pD_Delay * pH_D_DH * pC_D),
                        ("DLU", pD_Delay * pL_D_DH * pU_D),
                        ("DLC", pD_Delay * pL_D_DH * pC_D),
                        ("SHU", pS_Delay * pH_S_DH * pU_S),
                        ("SHC", pS_Delay * pH_S_DH * pC_S),
                        ("SLU", pS_Delay * pL_S_DH * pU_S),
                        ("SLC", pS_Delay * pL_S_DH * pC_S),
                    ]
                )
            else:
                raise ValueError("Invalid control for DHU at t>0.")

        elif x == "DHC":
            if y == "Start":
                return mkSimpleProb(
                    [
                        ("DHC", pD_Start * pH_D_DH),
                        ("DLC", pD_Start * pL_D_DH),
                        ("SHC", pS_Start * pH_S_DH),
                        ("SLC", pS_Start * pL_S_DH),
                    ]
                )
            elif y == "Delay":
                return mkSimpleProb(
                    [
                        ("DHC", pD_Delay * pH_D_DH),
                        ("DLC", pD_Delay * pL_D_DH),
                        ("SHC", pS_Delay * pH_S_DH),
                        ("SLC", pS_Delay * pL_S_DH),
                    ]
                )
            else:
                raise ValueError("Invalid control for DHC at t>0.")

        elif x == "DLU":
            if y == "Start":
                return mkSimpleProb(
                    [
                        ("DHU", pD_Start * pH_D_DL * pU_D),
                        ("DHC", pD_Start * pH_D_DL * pC_D),
                        ("DLU", pD_Start * pL_D_DL * pU_D),
                        ("DLC", pD_Start * pL_D_DL * pC_D),
                        ("SHU", pS_Start * pH_S_DL * pU_S),
                        ("SHC", pS_Start * pH_S_DL * pC_S),
                        ("SLU", pS_Start * pL_S_DL * pU_S),
                        ("SLC", pS_Start * pL_S_DL * pC_S),
                    ]
                )
            elif y == "Delay":
                return mkSimpleProb(
                    [
                        ("DHU", pD_Delay * pH_D_DL * pU_D),
                        ("DHC", pD_Delay * pH_D_DL * pC_D),
                        ("DLU", pD_Delay * pL_D_DL * pU_D),
                        ("DLC", pD_Delay * pL_D_DL * pC_D),
                        ("SHU", pS_Delay * pH_S_DL * pU_S),
                        ("SHC", pS_Delay * pH_S_DL * pC_S),
                        ("SLU", pS_Delay * pL_S_DL * pU_S),
                        ("SLC", pS_Delay * pL_S_DL * pC_S),
                    ]
                )
            else:
                raise ValueError("Invalid control for DLU at t>0.")

        elif x == "DLC":
            if y == "Start":
                return mkSimpleProb(
                    [
                        ("DHC", pD_Start * pH_D_DL),
                        ("DLC", pD_Start * pL_D_DL),
                        ("SHC", pS_Start * pH_S_DL),
                        ("SLC", pS_Start * pL_S_DL),
                    ]
                )
            elif y == "Delay":
                return mkSimpleProb(
                    [
                        ("DHC", pD_Delay * pH_D_DL),
                        ("DLC", pD_Delay * pL_D_DL),
                        ("SHC", pS_Delay * pH_S_DL),
                        ("SLC", pS_Delay * pL_S_DL),
                    ]
                )
            else:
                raise ValueError("Invalid control for DLC at t>0.")

        elif x == "SHU":
            if y is None:
                return mkSimpleProb(
                    [
                        ("SHU", pH_S_SH * pU_S),
                        ("SHC", pH_S_SH * pC_S),
                        ("SLU", pL_S_SH * pU_S),
                        ("SLC", pL_S_SH * pC_S),
                    ]
                )
            else:
                raise ValueError("Invalid control for SHU at t>0 (should be None).")

        elif x == "SHC":
            if y is None:
                return mkSimpleProb(
                    [
                        ("SHC", pH_S_SH),
                        ("SLC", pL_S_SH),
                    ]
                )
            else:
                raise ValueError("Invalid control for SHC at t>0.")

        elif x == "SLU":
            if y is None:
                return mkSimpleProb(
                    [
                        ("SHU", pH_S_SL * pU_S),
                        ("SHC", pH_S_SL * pC_S),
                        ("SLU", pL_S_SL * pU_S),
                        ("SLC", pL_S_SL * pC_S),
                    ]
                )
            else:
                raise ValueError("Invalid control for SLU at t>0.")

        elif x == "SLC":
            if y is None:
                return mkSimpleProb(
                    [
                        ("SHC", pH_S_SL),
                        ("SLC", pL_S_SL),
                    ]
                )
            else:
                raise ValueError("Invalid control for SLC at t>0.")

        else:
            raise ValueError("Unexpected state at t>0.")

        # Testing the transition function.

"""
test = nextFunc(0, "DHU", "Start")
for k, v in test.items():
    print(k, v) 

print("\nSum of all probabilities: ", sum(test.values()))
"""

# Reward function.
def reward(t: int, x: str, y: str, next_x: str) -> int:
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
    return 1.0 if next_x in ["DHU", "SHU"] else 0.0

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

"""
# Test of val function.
ps_test_start = [
    {"DHU": "Start"},
    {
        "DHU": "Start",
        "DHC": "Start",
        "DLU": "Start",
        "DLC": "Start",
        "SHU": None,
        "SHC": None,
        "SLU": None,
        "SLC": None,
    },
]

ps_test_delay = [
    {"DHU": "Delay"},
    {
        "DHU": "Delay",
        "DHC": "Delay",
        "DLU": "Delay",
        "DLC": "Delay",
        "SHU": None,
        "SHC": None,
        "SLU": None,
        "SLC": None,
    },
]

print(val(0, ps_test_start, "DHU"), val(0, ps_test_delay, "DHU"))
"""

# Computes the best single policy to add to an existing policy sequence.
def bestExt(t: int, ps_tail: list[dict[str, str]]) -> dict[str, str]:
    policy = dict()

    for state in states:
        best_value = -np.inf
        best_action = None

        # For each available action in the current state
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
    # Added the return of No Action instead of None as a string, mainly for testing purposes.
    if(b == None):
        b = "No Action"
    vb = val(t, [p] + ps, x)
    return f"Horizon, best, value : {n}, {b}, {vb}"

""" 
# Computing the best decision for different decision horizons.
bests = []
for i in range(1, 8):
    bests.append(best(0, i, "DHU"))

for b in bests:
    print(b) """

# For comparing outputs with those from the Idris version.
def run_best(x, y, state):
    result = best(x, y, state)
    print(result)

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

    # Comparing mMeas values to those of the article


def run_best(x, y, state):
    result = best(x, y, state)
    print(result)
    
    
""" 
# Comparing mMeas values to those of the article
print(mMeas(0, 4, "SHU"))
print(mMeas(0, 6, "SLC"))
print(mMeas(0, 7, "DHU"))
print(mMeas(1, 7, "DHU"))
print(mMeas(3, 7, "DHU"))
"""
