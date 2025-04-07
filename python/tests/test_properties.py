import math
import re
from enum import Enum, auto

import pytest
from hypothesis import given
from hypothesis import strategies as st
from src.application.theory import SDP, Action

"""
Testing the property of the SDP using pythons "hypothesis".
This test file aims to test and assure the properties of the functions defined by SDP.

Python Hypothesis 
- @given(...)                       = Hypothesis decorator for automatically generates test cases.
- st.integers(min_value=0)          = Randomly generates integer time steps (t).
- st.sampled_from(MySDP().states()) = Randomly picks a state from the list of valid states.

"""

### TO RUN (requires pytest -> pip install pytest) ###
# navigate to: ruu-thesis-project-port/ruu-thesis-project-port/python $
# run: pytest -s tests/test_properties.py -v
#

""" 
# EXPERIMENTAL, UNDER CONSTRUCTION:
@pytest.fixture
def sdp_instance():
    #Fixture to provide an SDP instance. Replace with different SDP instances as needed.
    from .test_SDP import TestSDP  # Import your SDP class

    return TestSDP()  # Instantiate the SDP instance """


# ==================== Test SDP Implementation ====================


# Declare all states of the SDP below:
class State(Enum):
    DHU = auto()
    DHC = auto()
    DLU = auto()
    DLC = auto()
    SHU = auto()
    SHC = auto()
    SLU = auto()
    SLC = auto()

# Declare all actions of the SDP below:
class Action(Enum):
    Start = auto()
    Delay = auto()

# Define transition probabilities below:
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

class MatterMost(SDP):
    @property
    def states(self) -> list[State]:
        return list(State)

    # Function that returns the possible actions in any allowed state.
    def actions(self, t: int, x: Enum) -> list[str] | list[None]:
        if x in [State.DHU, State.DHC, State.DLU, State.DLC]:
            return [Action.Start, Action.Delay]
        elif x in [State.SHU, State.SHC, State.SLU, State.SLC]:
            return [None]
        else:
            raise ValueError(f"Invalid State: '{x}'.")


    # Next takes in timestep t, state x, and action (control) y.
    def nextFunc(self, t: int, x: State, y: State) -> dict[State, float]:
        # "y" can be Action.Start, Action.Delay, or None for S-states.
        if t < 0 or type(t) != int:
            raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")
        if t == 0:
            # CASE: t == 0
            if x == State.DHU:
                if y == Action.Start:
                    return self.mkSimpleProb(
                        [
                            (State.DHU, pD_Start * pH_D_DH * pU_D_0),
                            (State.DHC, pD_Start * pH_D_DH * pC_D_0),
                            (State.DLU, pD_Start * pL_D_DH * pU_D_0),
                            (State.DLC, pD_Start * pL_D_DH * pC_D_0),
                            (State.SHU, pS_Start * pH_S_DH * pU_S_0),
                            (State.SHC, pS_Start * pH_S_DH * pC_S_0),
                            (State.SLU, pS_Start * pL_S_DH * pU_S_0),
                            (State.SLC, pS_Start * pL_S_DH * pC_S_0),
                        ]
                    )
                elif y == Action.Delay:
                    return self.mkSimpleProb(
                        [
                            (State.DHU, pD_Delay * pH_D_DH * pU_D_0),
                            (State.DHC, pD_Delay * pH_D_DH * pC_D_0),
                            (State.DLU, pD_Delay * pL_D_DH * pU_D_0),
                            (State.DLC, pD_Delay * pL_D_DH * pC_D_0),
                            (State.SHU, pS_Delay * pH_S_DH * pU_S_0),
                            (State.SHC, pS_Delay * pH_S_DH * pC_S_0),
                            (State.SLU, pS_Delay * pL_S_DH * pU_S_0),
                            (State.SLC, pS_Delay * pL_S_DH * pC_S_0),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for DHU at t=0.")

            elif x == State.DHC:
                if y == Action.Start:
                    return self.mkSimpleProb(
                        [
                            (State.DHC, pD_Start * pH_D_DH),
                            (State.DLC, pD_Start * pL_D_DH),
                            (State.SHC, pS_Start * pH_S_DH),
                            (State.SLC, pS_Start * pL_S_DH),
                        ]
                    )
                elif y == Action.Delay:
                    return self.mkSimpleProb(
                        [
                            (State.DHC, pD_Delay * pH_D_DH),
                            (State.DLC, pD_Delay * pL_D_DH),
                            (State.SHC, pS_Delay * pH_S_DH),
                            (State.SLC, pS_Delay * pL_S_DH),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for DHC at t=0.")

            elif x == State.DLU:
                if y == Action.Start:
                    return self.mkSimpleProb(
                        [
                            (State.DHU, pD_Start * pH_D_DL * pU_D_0),
                            (State.DHC, pD_Start * pH_D_DL * pC_D_0),
                            (State.DLU, pD_Start * pL_D_DL * pU_D_0),
                            (State.DLC, pD_Start * pL_D_DL * pC_D_0),
                            (State.SHU, pS_Start * pH_S_DL * pU_S_0),
                            (State.SHC, pS_Start * pH_S_DL * pC_S_0),
                            (State.SLU, pS_Start * pL_S_DL * pU_S_0),
                            (State.SLC, pS_Start * pL_S_DL * pC_S_0),
                        ]
                    )
                elif y == Action.Delay:
                    return self.mkSimpleProb(
                        [
                            (State.DHU, pD_Delay * pH_D_DL * pU_D_0),
                            (State.DHC, pD_Delay * pH_D_DL * pC_D_0),
                            (State.DLU, pD_Delay * pL_D_DL * pU_D_0),
                            (State.DLC, pD_Delay * pL_D_DL * pC_D_0),
                            (State.SHU, pS_Delay * pH_S_DL * pU_S_0),
                            (State.SHC, pS_Delay * pH_S_DL * pC_S_0),
                            (State.SLU, pS_Delay * pL_S_DL * pU_S_0),
                            (State.SLC, pS_Delay * pL_S_DL * pC_S_0),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for DLU at t=0.")

            elif x == State.DLC:
                if y == Action.Start:
                    return self.mkSimpleProb(
                        [
                            (State.DHC, pD_Start * pH_D_DL),
                            (State.DLC, pD_Start * pL_D_DL),
                            (State.SHC, pS_Start * pH_S_DL),
                            (State.SLC, pS_Start * pL_S_DL),
                        ]
                    )
                elif y == Action.Delay:
                    return self.mkSimpleProb(
                        [
                            (State.DHC, pD_Delay * pH_D_DL),
                            (State.DLC, pD_Delay * pL_D_DL),
                            (State.SHC, pS_Delay * pH_S_DL),
                            (State.SLC, pS_Delay * pL_S_DL),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for DLC at t=0.")

            elif x == State.SHU:
                # In Idris: Theory.next Z SHU () = mkSimpleProb [...]
                # The control is '()', i.e. a unit (we represent it as None in Python).
                if y is None:
                    return self.mkSimpleProb(
                        [
                            (State.SHU, pH_S_SH * pU_S_0),
                            (State.SHC, pH_S_SH * pC_S_0),
                            (State.SLU, pL_S_SH * pU_S_0),
                            (State.SLC, pL_S_SH * pC_S_0),
                        ]
                    )
                else:
                    raise ValueError(f"Invalid control for SHU at t=0 (should be None), actual: {y}" )

            elif x == State.SHC:
                if y is None:
                    return self.mkSimpleProb(
                        [
                            (State.SHC, pH_S_SH),
                            (State.SLC, pL_S_SH),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for SHC at t=0.")

            elif x == State.SLU:
                if y is None:
                    return self.mkSimpleProb(
                        [
                            (State.SHU, pH_S_SL * pU_S_0),
                            (State.SHC, pH_S_SL * pC_S_0),
                            (State.SLU, pL_S_SL * pU_S_0),
                            (State.SLC, pL_S_SL * pC_S_0),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for SLU at t=0.")

            elif x == State.SLC:
                if y is None:
                    return self.mkSimpleProb(
                        [
                            (State.SHC, pH_S_SL),
                            (State.SLC, pL_S_SL),
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

            if x == State.DHU:
                if y == Action.Start:
                    return self.mkSimpleProb(
                        [
                            (State.DHU, pD_Start * pH_D_DH * pU_D),
                            (State.DHC, pD_Start * pH_D_DH * pC_D),
                            (State.DLU, pD_Start * pL_D_DH * pU_D),
                            (State.DLC, pD_Start * pL_D_DH * pC_D),
                            (State.SHU, pS_Start * pH_S_DH * pU_S),
                            (State.SHC, pS_Start * pH_S_DH * pC_S),
                            (State.SLU, pS_Start * pL_S_DH * pU_S),
                            (State.SLC, pS_Start * pL_S_DH * pC_S),
                        ]
                    )
                elif y == Action.Delay:
                    return self.mkSimpleProb(
                        [
                            (State.DHU, pD_Delay * pH_D_DH * pU_D),
                            (State.DHC, pD_Delay * pH_D_DH * pC_D),
                            (State.DLU, pD_Delay * pL_D_DH * pU_D),
                            (State.DLC, pD_Delay * pL_D_DH * pC_D),
                            (State.SHU, pS_Delay * pH_S_DH * pU_S),
                            (State.SHC, pS_Delay * pH_S_DH * pC_S),
                            (State.SLU, pS_Delay * pL_S_DH * pU_S),
                            (State.SLC, pS_Delay * pL_S_DH * pC_S),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for DHU at t>0.")

            elif x == State.DHC:
                if y == Action.Start:
                    return self.mkSimpleProb(
                        [
                            (State.DHC, pD_Start * pH_D_DH),
                            (State.DLC, pD_Start * pL_D_DH),
                            (State.SHC, pS_Start * pH_S_DH),
                            (State.SLC, pS_Start * pL_S_DH),
                        ]
                    )
                elif y == Action.Delay:
                    return self.mkSimpleProb(
                        [
                            (State.DHC, pD_Delay * pH_D_DH),
                            (State.DLC, pD_Delay * pL_D_DH),
                            (State.SHC, pS_Delay * pH_S_DH),
                            (State.SLC, pS_Delay * pL_S_DH),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for DHC at t>0.")

            elif x == State.DLU:
                if y == Action.Start:
                    return self.mkSimpleProb(
                        [
                            (State.DHU, pD_Start * pH_D_DL * pU_D),
                            (State.DHC, pD_Start * pH_D_DL * pC_D),
                            (State.DLU, pD_Start * pL_D_DL * pU_D),
                            (State.DLC, pD_Start * pL_D_DL * pC_D),
                            (State.SHU, pS_Start * pH_S_DL * pU_S),
                            (State.SHC, pS_Start * pH_S_DL * pC_S),
                            (State.SLU, pS_Start * pL_S_DL * pU_S),
                            (State.SLC, pS_Start * pL_S_DL * pC_S),
                        ]
                    )
                elif y == Action.Delay:
                    return self.mkSimpleProb(
                        [
                            (State.DHU, pD_Delay * pH_D_DL * pU_D),
                            (State.DHC, pD_Delay * pH_D_DL * pC_D),
                            (State.DLU, pD_Delay * pL_D_DL * pU_D),
                            (State.DLC, pD_Delay * pL_D_DL * pC_D),
                            (State.SHU, pS_Delay * pH_S_DL * pU_S),
                            (State.SHC, pS_Delay * pH_S_DL * pC_S),
                            (State.SLU, pS_Delay * pL_S_DL * pU_S),
                            (State.SLC, pS_Delay * pL_S_DL * pC_S),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for DLU at t>0.")

            elif x == State.DLC:
                if y == Action.Start:
                    return self.mkSimpleProb(
                        [
                            (State.DHC, pD_Start * pH_D_DL),
                            (State.DLC, pD_Start * pL_D_DL),
                            (State.SHC, pS_Start * pH_S_DL),
                            (State.SLC, pS_Start * pL_S_DL),
                        ]
                    )
                elif y == Action.Delay:
                    return self.mkSimpleProb(
                        [
                            (State.DHC, pD_Delay * pH_D_DL),
                            (State.DLC, pD_Delay * pL_D_DL),
                            (State.SHC, pS_Delay * pH_S_DL),
                            (State.SLC, pS_Delay * pL_S_DL),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for DLC at t>0.")

            elif x == State.SHU:
                if y is None:
                    return self.mkSimpleProb(
                        [
                            (State.SHU, pH_S_SH * pU_S),
                            (State.SHC, pH_S_SH * pC_S),
                            (State.SLU, pL_S_SH * pU_S),
                            (State.SLC, pL_S_SH * pC_S),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for SHU at t>0 (should be None).")

            elif x == State.SHC:
                if y is None:
                    return self.mkSimpleProb(
                        [
                            (State.SHC, pH_S_SH),
                            (State.SLC, pL_S_SH),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for SHC at t>0.")

            elif x == State.SLU:
                if y is None:
                    return self.mkSimpleProb(
                        [
                            (State.SHU, pH_S_SL * pU_S),
                            (State.SHC, pH_S_SL * pC_S),
                            (State.SLU, pL_S_SL * pU_S),
                            (State.SLC, pL_S_SL * pC_S),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for SLU at t>0.")

            elif x == State.SLC:
                if y is None:
                    return self.mkSimpleProb(
                        [
                            (State.SHC, pH_S_SL),
                            (State.SLC, pL_S_SL),
                        ]
                    )
                else:
                    raise ValueError("Invalid control for SLC at t>0.")

            else:
                raise ValueError("Unexpected state at t>0.")

            # Testing the transition function.

    def reward(self, t: int, x: State, y: Action, next_x: State) -> int:
        # Value is added for transitioning into states which do not have low economic
        # output and at the same time are not comitted to severe future climate change.
        if t < 0 or type(t) != int:
            raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")
        if x not in self.states:
            raise ValueError(f"Invalid state: '{x}'")
        if y not in self.actions(t, x):
            raise ValueError(f"Invalid action: '{y}'")
        if next_x not in self.states:
            raise ValueError(f"Invalid next state: '{next_x}'")
        return 1.0 if next_x in [State.DHU, State.SHU] else 0.0



sdp_instance = MatterMost()


# ==================== Property Tests: action  ====================


# Tests if the actions function always returns a list, t is irrelevant.
@given(st.integers(min_value=0, max_value=8), st.sampled_from(sdp_instance.states))
def test_actions_return_list(t, state):  # `t` is provided by Hypothesis
    actions = sdp_instance.actions(t, state)
    assert isinstance(actions, list)


# Tests that the elements in the list are of type Action
@given(st.integers(min_value=0, max_value=8), st.sampled_from(sdp_instance.states))
def test_actions_return_type_action(t, state):
    actions = sdp_instance.actions(t, state)
    assert (all(isinstance(a, Action)) or a is None for a in actions)


# Test that the function will behave the same for same input
@given(st.integers(min_value=0), st.sampled_from(State))
def test_actions_deterministic(t, x):
    result1 = sdp_instance.actions(t, x)
    result2 = sdp_instance.actions(t, x)
    assert result1 == result2


# ==================== Property Tests: nextFunc  ====================


# Test that nextFunc gives back a dictionary and that the dictionary if of type "dict[State, float]"
@given(
    st.integers(min_value=1, max_value=8),
    st.sampled_from(sdp_instance.states),
)
def test_nextFunc_return_dict(t, x):
    y = sdp_instance.actions(t, x)
    next = sdp_instance.nextFunc(t, x, y[0])
    assert isinstance(next, dict)
    assert all(
        isinstance(k, State) for k in next.keys()
    ), f"Keys are not all State instances: {next.keys()}"
    assert all(
        isinstance(v, float) for v in next.values()
    ), f"Values are not all float: {next.values()}"


# Test that Probabilities given from next always sum to 1
@given(
    st.integers(min_value=1, max_value=8),
    st.sampled_from(sdp_instance.states),
)
def test_nextFunc_probabilities_sum_1(t, x):
    y = sdp_instance.actions(t, x)
    next = sdp_instance.nextFunc(t, x, y[0])
    total = sum(next.values())
    assert abs(total - 1.0) < 1e-7


# Test that all probabilities are larger than 0
@given(
    st.integers(min_value=0, max_value=8),
    st.sampled_from(sdp_instance.states),
)
def test_nextFunc_no_negative_probs(t, x):
    y = sdp_instance.actions(t, x)
    next = sdp_instance.nextFunc(t, x, y[0])
    assert all(v >= 0 for v in next.values())


# Test that all states returned from the next function are legitimate states (member of the enum class State)
@given(
    st.integers(min_value=0, max_value=8),
    st.sampled_from(sdp_instance.states),
)
def test_nextFunc_valid_states(t, x):
    y = sdp_instance.actions(t, x)
    next = sdp_instance.nextFunc(t, x, y[0])
    assert set(next.keys()).issubset(set(sdp_instance.states))


# Test that we always get the same dictionary (same states and probabilities) given the exact same input.
@given(
    st.integers(min_value=0, max_value=8),
    st.sampled_from(sdp_instance.states),
)
def test_nextFunc_determinism(t, x):
    y = sdp_instance.actions(t, x)
    result1 = sdp_instance.nextFunc(t, x, y[0])
    result2 = sdp_instance.nextFunc(t, x, y[0])

    assert result1 == result2, f"Function is non-deterministic: {result1} != {result2}"


# ==================== Property Tests: reward  ====================


# Test to ensure reward always returns an integer
@given(
    st.integers(min_value=0, max_value=8),
    st.sampled_from(sdp_instance.states),
    st.sampled_from(sdp_instance.states),
)
def test_reward_return_int(t: int, x: State, next_x: State):
    y = sdp_instance.actions(t, x)
    result = sdp_instance.reward(t, x, y[0], next_x)
    assert isinstance(result, float)


# Test that reward always returns a positive integer (no multiplication with 0)
@given(
    st.integers(min_value=0, max_value=8),
    st.sampled_from(sdp_instance.states),
    st.sampled_from(sdp_instance.states),
)
def test_reward_return_positive_int(t: int, x: State, next_x: State):
    y = sdp_instance.actions(t, x)
    result = sdp_instance.reward(t, x, y[0], next_x)
    assert result >= 0


# Test that reward always gives the same reward given the same inputs:
@given(
    st.integers(min_value=1, max_value=8),
    st.sampled_from(sdp_instance.states),
    st.sampled_from(sdp_instance.states),
)
def test_reward_stochastic(t: int, x: State, next_x: State):
    y = sdp_instance.actions(t, x)
    result1 = sdp_instance.reward(t, x, y[0], next_x)
    result2 = sdp_instance.reward(t, x, y[0], next_x)
    assert result1 == result2


# ==================== Property Tests: mkSimpleProb  ====================


# Test: Valid Probabilities Sum to 1
def test_mkSimpleProb_valid():
    x = sdp_instance.states[0]
    y = sdp_instance.states[1]
    test_data = [(x, 0.4), (y, 0.6)]
    result = sdp_instance.mkSimpleProb(test_data)
    assert sum(result.values()) == 1.0


# Test: Rejects Negative Probabilities
def test_mkSimpleProb_no_negative_probs():
    x = sdp_instance.states[0]
    y = sdp_instance.states[1]
    test_data = [(x, -0.1), (y, 1.1)]
    with pytest.raises(ValueError, match="No negative probabilities allowed."):
        sdp_instance.mkSimpleProb(test_data)


# Test: Rejects Probabilities That Do Not Sum to 1
def test_mkSimpleProb_invalid_sum():
    x = sdp_instance.states[0]
    y = sdp_instance.states[1]
    test_data = [(x, 0.3), (y, 0.3)]
    with pytest.raises(ValueError, match="Probabilities do not sum to 1"):
        sdp_instance.mkSimpleProb(test_data)
        

""" 
@given(
    st.lists(
        st.tuples(
            st.sampled_from(sdp_instance.states),  # Pick valid states
            st.floats(min_value=0, max_value=1)    # Only non-negative probabilities
        ), 
        min_size=1
    )
)
def test_mkSimpleProb_hypothesis(pairs):
    total = sum(p[1] for p in pairs)
    if total > 0:  # Normalize if sum > 0 (otherwise, function would always raise ValueError)
        normalized_pairs = [(s, p / total) for s, p in pairs]
        result = sdp_instance.mkSimpleProb(normalized_pairs)
        assert sum(result.values()) == 1.0 """



# ==================== Property Tests: val  ====================
# def val(self, t: int, ps: list[dict[State, Action]], x: State) -> float:

# Test that the return of val is a float
""" @given(
    st.integers(min_value=0, max_value=5),  # Small t to limit recursion depth
    st.lists(
        st.dictionaries(
            keys=st.sampled_from(sdp_instance.states),
            values=st.sampled_from(list(Action)),
            min_size = 1
        ),
        min_size=1,
        max_size=5,  # Control recursion depth by limiting `ps` size
    ),
    st.sampled_from(sdp_instance.states),
)
def test_val_return(t: int, ps: list[dict[State, Action]], nx: State):
    if ps and nx not in ps[0]:  # Ensure valid key access
        ps[0][nx] = Action.Start  # Assign a default action
    result = sdp_instance.val(t, ps, nx)
    assert isinstance(result, float) 
 """
# ==================== Property Tests: bestExt  ====================

# Test that bestExt returns a policy (dict[State, Action])
@given(
    st.integers(min_value=0, max_value=5),
    st.sampled_from(sdp_instance.states)
)
def test_bestExt_return_value(t: int, state: State):
    action = sdp_instance.actions(t, state)
    ps_tail = [dict([(state, action[0])])]
    result = sdp_instance.bestExt(t, ps_tail)
    assert isinstance(result, dict)
    assert ps_tail  

# The output policy should only assign valid actions to each state.

# That means policy[state] should be in self.actions(t, state).
@given(st.integers(min_value=0, max_value=5), st.sampled_from(sdp_instance.states))
def test_bestExt_valid_actions(t: int, state: State):
    
    action = sdp_instance.actions(t, state)
    ps_tail = [dict([(state, action[0])])]
    
    policy = sdp_instance.bestExt(t, ps_tail)
    for state, action in policy.items():
        assert action in sdp_instance.actions(t, state)

# Test that running bestExt with the same inputs should return the same policy
@given(st.integers(min_value=0, max_value=5), st.sampled_from(sdp_instance.states))
def test_bestExt_deterministic(t, state):

    action = sdp_instance.actions(t, state)
    ps_tail = [dict([(state, action[0])])]
    
    result1 = sdp_instance.bestExt(t, ps_tail)
    result2 = sdp_instance.bestExt(t, ps_tail)
    assert result1 == result2

# This test tangents the "val" function, but bestExt should return the best (optimal) result given from val:
# If val(t, [p] + ps_tail, state) is strictly better for some action a', bestExt should return a'.
@given(st.integers(min_value=0, max_value=100), st.sampled_from(sdp_instance.states))
def test_bestExt_optimality(t, state):
    
    action = sdp_instance.actions(t, state)
    ps_tail = [dict([(state, action[0])])]
    
    policy = sdp_instance.bestExt(t, ps_tail)
    for state, action in policy.items():
        best_value = sdp_instance.val(t, [{state: action}] + ps_tail, state)
        for other_action in sdp_instance.actions(t, state):
            value = sdp_instance.val(t, [{state: other_action}] + ps_tail, state)
            assert value <= best_value
            
            
            
# Test that the function work consistently over different t values
@given(st.integers(min_value=0, max_value=10), st.sampled_from(sdp_instance.states))
def test_bestExt_stability(t, state):
    
    action = sdp_instance.actions(t, state)
    ps_tail = [dict([(state, action[0])])]
    
    policy1 = sdp_instance.bestExt(t, ps_tail)
    policy2 = sdp_instance.bestExt(t + 1, ps_tail)
    assert policy1.keys() == policy2.keys()

# ==================== Property Tests: worstExt  ====================


# ==================== Property Tests: bi  ====================


# Test that the length of the computed sequence steps dont supersedes the given limit/horizon
@given(st.integers(min_value=0, max_value=5), st.integers(min_value=0, max_value=5))
def test_bi_length(t, n):
    result = sdp_instance.bi(t, n)
    assert len(result) == n


# Test that the bi function upholds its recursive structure, by checking if the next value in the "stack" is different from the previous one.
@given(st.integers(min_value=0, max_value=5), st.integers(min_value=0, max_value=5))
def test_bi_recursive_structure(t, n):
    result = sdp_instance.bi(t, n)
    for i in range(1, n):
        assert result[i - 1] != result[i]  # TODO: For some reason bi returns the exat same policy sequence


# Test that the return values are of correct type (State, Action)
@given(st.integers(min_value=0, max_value=5), st.integers(min_value=0, max_value=5))
def test_bi_valid_returns(t, n):
    result = sdp_instance.bi(t, n)
    for p in result:
        for state, action in p.items():
            assert state in sdp_instance.states
            assert action in sdp_instance.actions(t, state)  # Ensure valid action


# Testing the base case returning an empty list
@given(st.integers(min_value=0, max_value=5))
def test_bi_base_case(t):
    result = sdp_instance.bi(t, 0)
    assert result == []


# Testing that the optimal policy is the same given same input type
@given(st.integers(min_value=0, max_value=5), st.integers(min_value=0, max_value=5))
def test_bi_consistency(t, n):
    result1 = sdp_instance.bi(t, n)
    result2 = sdp_instance.bi(t, n)
    assert result1 == result2


# Test that the next recursion contains State and Action that are in the previous recursive step, asserting correct recursion.
@given(st.integers(min_value=0, max_value=5), st.integers(min_value=1, max_value=5))
def test_bi_monotonicity_consistency(t, n):
    "Ensure that policies evolve logically based on previous steps."
    result = sdp_instance.bi(t, n)

    for i in range(1, n):
        prev_policy = result[i - 1]
        curr_policy = result[i]

        # Ensure that the current policy is not completely unrelated to the previous one
        policies_consistent = any(
            prev_policy.get(state) == curr_policy.get(state) for state in prev_policy
        )

        assert (
            policies_consistent
        ), f"Policies at step {i} and {i-1} do not show logical evolution."


# ==================== Property Tests: best  ====================


# Test that the function returns a correct error response to a horizon < 1
def test_best_invalid_horizon():
    x = sdp_instance.states[0]
    with pytest.raises(ValueError, match="The horizon must be greater than zero!"):
        sdp_instance.best(0, 0, x)


# Test that the return string matches the format: "Horizon, best, value : {n}, {b}, {vb}"
def test_best_output_format():
    x = sdp_instance.states[0]
    result = sdp_instance.best(0, 2, x)
    assert re.match(
        r"Horizon, best, value : \d+, .*?, .*?", result
    ), f"Unexpected output format: {result}"


# Test that given a policy sequence, the extended sequence should include the subset of the previous sequence.
# Example: sequence = [1,2,3], extended sequence = [1,2,3,4,5], the extended sequence contains the subset "[1,2,3]"
def test_best_policy_consistency():
    x = sdp_instance.states[0]
    ps = sdp_instance.bi(1, 2)
    extended_policy = sdp_instance.bestExt(1, ps)
    result = sdp_instance.best(1, 3, x)
    assert str(extended_policy[x]) in result


# Test that makes sure an increase in horizon changes the result provided by the best function. This test specifically focuses on the string return behavour.
def test_best_horizon_increasing():
    result_1 = sdp_instance.best(0, 1, sdp_instance.states[0])
    result_2 = sdp_instance.best(0, 2, sdp_instance.states[0])

    assert result_1 != result_2


# ==================== Property Tests: mMeas  ====================


# Test that the return type of mMeas is correct
# Test that the function always returns a value between 0 and 1
@given(
    st.integers(min_value=0, max_value=5),
    st.integers(min_value=1, max_value=5),
    st.sampled_from(sdp_instance.states),
)
def test_mMeas_output_range(t, n, x):
    x = sdp_instance.states[0]
    result = sdp_instance.mMeas(t, n, x)
    assert isinstance(result, float)


# Test that the function always returns a value between 0 and 1
@given(
    st.integers(min_value=0, max_value=5),
    st.integers(min_value=1, max_value=5),
    st.sampled_from(sdp_instance.states),
)
def test_mMeas_output_range(t, n, x):
    x = sdp_instance.states[0]
    result = sdp_instance.mMeas(t, n, x)
    assert 0 <= result <= 1, f"mMeas({t}, {n}, {x}) = {result}, expected in [0,1]"


def test_mMeas_monotonicity():
    x = sdp_instance.states[0]
    t = 1
    prev_value = sdp_instance.mMeas(t, 7, x)
    for n in range(6, 0, -1):
        curr_value = sdp_instance.mMeas(t, n, x)
        assert curr_value <= prev_value or math.isclose(
            curr_value, prev_value, abs_tol=1e-10
        ), f"mMeas({t}, {n}, {x}) increased unexpectedly: {prev_value} → {curr_value}"
