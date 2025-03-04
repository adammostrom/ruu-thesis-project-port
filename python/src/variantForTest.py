import numpy as np

states = ["DHU", "DHC", "DLU", "DLC", "SHU", "SHC", "SLU", "SLC"]
# actions = 

# Defining the probabilities for the transition function
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


def mkSimpleProb(pairs: list[tuple[str, float]]) -> dict[str, float]:
    dist: dict[str, float] = {}
    for (st, pr) in pairs:
        if pr > 0:
            dist[st] = pr
    return dist

def next_fun(t: int, x: str, y: str) -> dict[str, float]:
    # "y" can be "Start", "Delay", or None for S-states.
    if t == 0:
        # CASE: t == 0
        if x == "DHU":
            if y == "Start":
                return mkSimpleProb([
                    ("DHU", pD_Start * pH_D_DH * pU_D_0),
                    ("DHC", pD_Start * pH_D_DH * pC_D_0),
                    ("DLU", pD_Start * pL_D_DH * pU_D_0),
                    ("DLC", pD_Start * pL_D_DH * pC_D_0),
                    ("SHU", pS_Start * pH_S_DH * pU_S_0),
                    ("SHC", pS_Start * pH_S_DH * pC_S_0),
                    ("SLU", pS_Start * pL_S_DH * pU_S_0),
                    ("SLC", pS_Start * pL_S_DH * pC_S_0),
                ])
            elif y == "Delay":
                return mkSimpleProb([
                    ("DHU", pD_Delay * pH_D_DH * pU_D_0),
                    ("DHC", pD_Delay * pH_D_DH * pC_D_0),
                    ("DLU", pD_Delay * pL_D_DH * pU_D_0),
                    ("DLC", pD_Delay * pL_D_DH * pC_D_0),
                    ("SHU", pS_Delay * pH_S_DH * pU_S_0),
                    ("SHC", pS_Delay * pH_S_DH * pC_S_0),
                    ("SLU", pS_Delay * pL_S_DH * pU_S_0),
                    ("SLC", pS_Delay * pL_S_DH * pC_S_0),
                ])
            else:
                raise ValueError("Invalid control for DHU at t=0.")
        
        elif x == "DHC":
            if y == "Start":
                return mkSimpleProb([
                    ("DHC", pD_Start * pH_D_DH),
                    ("DLC", pD_Start * pL_D_DH),
                    ("SHC", pS_Start * pH_S_DH),
                    ("SLC", pS_Start * pL_S_DH),
                ])
            elif y == "Delay":
                return mkSimpleProb([
                    ("DHC", pD_Delay * pH_D_DH),
                    ("DLC", pD_Delay * pL_D_DH),
                    ("SHC", pS_Delay * pH_S_DH),
                    ("SLC", pS_Delay * pL_S_DH),
                ])
            else:
                raise ValueError("Invalid control for DHC at t=0.")
        
        elif x == "DLU":
            if y == "Start":
                return mkSimpleProb([
                    ("DHU", pD_Start * pH_D_DL * pU_D_0),
                    ("DHC", pD_Start * pH_D_DL * pC_D_0),
                    ("DLU", pD_Start * pL_D_DL * pU_D_0),
                    ("DLC", pD_Start * pL_D_DL * pC_D_0),
                    ("SHU", pS_Start * pH_S_DL * pU_S_0),
                    ("SHC", pS_Start * pH_S_DL * pC_S_0),
                    ("SLU", pS_Start * pL_S_DL * pU_S_0),
                    ("SLC", pS_Start * pL_S_DL * pC_S_0),
                ])
            elif y == "Delay":
                return mkSimpleProb([
                    ("DHU", pD_Delay * pH_D_DL * pU_D_0),
                    ("DHC", pD_Delay * pH_D_DL * pC_D_0),
                    ("DLU", pD_Delay * pL_D_DL * pU_D_0),
                    ("DLC", pD_Delay * pL_D_DL * pC_D_0),
                    ("SHU", pS_Delay * pH_S_DL * pU_S_0),
                    ("SHC", pS_Delay * pH_S_DL * pC_S_0),
                    ("SLU", pS_Delay * pL_S_DL * pU_S_0),
                    ("SLC", pS_Delay * pL_S_DL * pC_S_0),
                ])
            else:
                raise ValueError("Invalid control for DLU at t=0.")
        
        elif x == "DLC":
            if y == "Start":
                return mkSimpleProb([
                    ("DHC", pD_Start * pH_D_DL),
                    ("DLC", pD_Start * pL_D_DL),
                    ("SHC", pS_Start * pH_S_DL),
                    ("SLC", pS_Start * pL_S_DL),
                ])
            elif y == "Delay":
                return mkSimpleProb([
                    ("DHC", pD_Delay * pH_D_DL),
                    ("DLC", pD_Delay * pL_D_DL),
                    ("SHC", pS_Delay * pH_S_DL),
                    ("SLC", pS_Delay * pL_S_DL),
                ])
            else:
                raise ValueError("Invalid control for DLC at t=0.")
        
        elif x == "SHU":
            # In Idris: Theory.next Z SHU () = mkSimpleProb [...]
            # The control is '()', i.e. a unit (we represent it as None in Python).
            if y is None:
                return mkSimpleProb([
                    ("SHU", pH_S_SH * pU_S_0),
                    ("SHC", pH_S_SH * pC_S_0),
                    ("SLU", pL_S_SH * pU_S_0),
                    ("SLC", pL_S_SH * pC_S_0),
                ])
            else:
                raise ValueError("Invalid control for SHU at t=0 (should be None).")
        
        elif x == "SHC":
            if y is None:
                return mkSimpleProb([
                    ("SHC", pH_S_SH),
                    ("SLC", pL_S_SH),
                ])
            else:
                raise ValueError("Invalid control for SHC at t=0.")
        
        elif x == "SLU":
            if y is None:
                return mkSimpleProb([
                    ("SHU", pH_S_SL * pU_S_0),
                    ("SHC", pH_S_SL * pC_S_0),
                    ("SLU", pL_S_SL * pU_S_0),
                    ("SLC", pL_S_SL * pC_S_0),
                ])
            else:
                raise ValueError("Invalid control for SLU at t=0.")
        
        elif x == "SLC":
            if y is None:
                return mkSimpleProb([
                    ("SHC", pH_S_SL),
                    ("SLC", pL_S_SL),
                ])
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
                return mkSimpleProb([
                    ("DHU", pD_Start * pH_D_DH * pU_D),
                    ("DHC", pD_Start * pH_D_DH * pC_D),
                    ("DLU", pD_Start * pL_D_DH * pU_D),
                    ("DLC", pD_Start * pL_D_DH * pC_D),
                    ("SHU", pS_Start * pH_S_DH * pU_S),
                    ("SHC", pS_Start * pH_S_DH * pC_S),
                    ("SLU", pS_Start * pL_S_DH * pU_S),
                    ("SLC", pS_Start * pL_S_DH * pC_S),
                ])
            elif y == "Delay":
                return mkSimpleProb([
                    ("DHU", pD_Delay * pH_D_DH * pU_D),
                    ("DHC", pD_Delay * pH_D_DH * pC_D),
                    ("DLU", pD_Delay * pL_D_DH * pU_D),
                    ("DLC", pD_Delay * pL_D_DH * pC_D),
                    ("SHU", pS_Delay * pH_S_DH * pU_S),
                    ("SHC", pS_Delay * pH_S_DH * pC_S),
                    ("SLU", pS_Delay * pL_S_DH * pU_S),
                    ("SLC", pS_Delay * pL_S_DH * pC_S),
                ])
            else:
                raise ValueError("Invalid control for DHU at t>0.")
        
        elif x == "DHC":
            if y == "Start":
                return mkSimpleProb([
                    ("DHC", pD_Start * pH_D_DH),
                    ("DLC", pD_Start * pL_D_DH),
                    ("SHC", pS_Start * pH_S_DH),
                    ("SLC", pS_Start * pL_S_DH),
                ])
            elif y == "Delay":
                return mkSimpleProb([
                    ("DHC", pD_Delay * pH_D_DH),
                    ("DLC", pD_Delay * pL_D_DH),
                    ("SHC", pS_Delay * pH_S_DH),
                    ("SLC", pS_Delay * pL_S_DH),
                ])
            else:
                raise ValueError("Invalid control for DHC at t>0.")
        
        elif x == "DLU":
            if y == "Start":
                return mkSimpleProb([
                    ("DHU", pD_Start * pH_D_DL * pU_D),
                    ("DHC", pD_Start * pH_D_DL * pC_D),
                    ("DLU", pD_Start * pL_D_DL * pU_D),
                    ("DLC", pD_Start * pL_D_DL * pC_D),
                    ("SHU", pS_Start * pH_S_DL * pU_S),
                    ("SHC", pS_Start * pH_S_DL * pC_S),
                    ("SLU", pS_Start * pL_S_DL * pU_S),
                    ("SLC", pS_Start * pL_S_DL * pC_S),
                ])
            elif y == "Delay":
                return mkSimpleProb([
                    ("DHU", pD_Delay * pH_D_DL * pU_D),
                    ("DHC", pD_Delay * pH_D_DL * pC_D),
                    ("DLU", pD_Delay * pL_D_DL * pU_D),
                    ("DLC", pD_Delay * pL_D_DL * pC_D),
                    ("SHU", pS_Delay * pH_S_DL * pU_S),
                    ("SHC", pS_Delay * pH_S_DL * pC_S),
                    ("SLU", pS_Delay * pL_S_DL * pU_S),
                    ("SLC", pS_Delay * pL_S_DL * pC_S),
                ])
            else:
                raise ValueError("Invalid control for DLU at t>0.")
        
        elif x == "DLC":
            if y == "Start":
                return mkSimpleProb([
                    ("DHC", pD_Start * pH_D_DL),
                    ("DLC", pD_Start * pL_D_DL),
                    ("SHC", pS_Start * pH_S_DL),
                    ("SLC", pS_Start * pL_S_DL),
                ])
            elif y == "Delay":
                return mkSimpleProb([
                    ("DHC", pD_Delay * pH_D_DL),
                    ("DLC", pD_Delay * pL_D_DL),
                    ("SHC", pS_Delay * pH_S_DL),
                    ("SLC", pS_Delay * pL_S_DL),
                ])
            else:
                raise ValueError("Invalid control for DLC at t>0.")
        
        elif x == "SHU":
            if y is None:
                return mkSimpleProb([
                    ("SHU", pH_S_SH * pU_S),
                    ("SHC", pH_S_SH * pC_S),
                    ("SLU", pL_S_SH * pU_S),
                    ("SLC", pL_S_SH * pC_S),
                ])
            else:
                raise ValueError("Invalid control for SHU at t>0 (should be None).")
        
        elif x == "SHC":
            if y is None:
                return mkSimpleProb([
                    ("SHC", pH_S_SH),
                    ("SLC", pL_S_SH),
                ])
            else:
                raise ValueError("Invalid control for SHC at t>0.")
        
        elif x == "SLU":
            if y is None:
                return mkSimpleProb([
                    ("SHU", pH_S_SL * pU_S),
                    ("SHC", pH_S_SL * pC_S),
                    ("SLU", pL_S_SL * pU_S),
                    ("SLC", pL_S_SL * pC_S),
                ])
            else:
                raise ValueError("Invalid control for SLU at t>0.")
        
        elif x == "SLC":
            if y is None:
                return mkSimpleProb([
                    ("SHC", pH_S_SL),
                    ("SLC", pL_S_SL),
                ])
            else:
                raise ValueError("Invalid control for SLC at t>0.")
        
        else:
            raise ValueError("Unexpected state at t>0.")
        
        
        
# Testing the transition function for a first time.
test = next_fun(0, "DHU", "Start")

print(test)

print(sum(test.values()))
for k, v in test.items():
    print(k, v)


def reward(t: int, x: str, y: str, next_x: str):
    return 1 if x == "SHU" else 0

# First test
print(reward(1, "SHU", "Start", "DHL"))

def val(t: int, n: int, ps: list[dict[str, str]], x: str) -> float:
    value = 0
    if len(ps) == 0:
        return value
    y = ps[0][x]
    m_next = next_fun(t, x, y)
    for x_prim, pr in m_next.items():
        value += (reward(t, x, y, x_prim) + val(t+1, n-1, ps[1:], x_prim)) * pr
    return value


ps_test = [{"DHU": "Start"}, 
           {"DHU": "Start", "DHC": "Delay",
            "DLU": "Start", "DLC": "Delay",
            "SHU": None, "SHC": None,
            "SLU": None, "SLC": None}]

test_val = val(0, 1, ps_test, "DHU")
print(test_val)



def best_ext(t: int, n: int, ps_tail) -> dict[str, str]:
    policy = dict()
    
    best_value = -np.inf
    best_action = None

    for state in states:
        if state in ["DHU", "DHC", "DLU", "DLC"]:
            actions = ["Start", "Delay"]
        else:
            policy[state] = None
            continue

        # For each available action in the current state
        for action in actions:
            # Calculate value of taking action in state
            value = val(t, n, ps_tail, state)
            # Choose the action with the highest expected value
            if value > best_value:
                best_value = value
                best_action = action
        
        policy[state] = best_action
        print("BEST_EXT - ps_tail", ps_tail)

    return policy
    

def bi(t: int, n: int) -> list[dict[str, str]]:
    if n == 0:
        return []
    else:
        ps_tail = bi(t + 1, n - 1)
        p = best_ext(t, n, ps_tail)
        print("BI - p", p)
        print("BI - ps_tail", ps_tail)
        return [p] + ps_tail
    


def best(t: int, n: int, x: str) -> str:
    ps = bi(t+1, n)
    p = best_ext(t, n, ps)
    b = p[x]
    vb = val(t, n, [p] + ps, x)
    return f"Horizon, best, value: {n+1} {b} {vb}"

test_best = best(0, 1, "DHU")

print(test_best)