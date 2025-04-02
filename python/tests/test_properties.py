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


# EXPERIMENTAL, UNDER CONSTRUCTION:
@pytest.fixture
def sdp_instance():
    """Fixture to provide an SDP instance. Replace with different SDP instances as needed."""
    from .test_SDP import TestSDP  # Import your SDP class

    return TestSDP()  # Instantiate the SDP instance


# ==================== Test SDP Implementation ====================

"""Define the possible states for the SDP"""


class State(Enum):
    S1 = auto()
    S2 = auto()
    S3 = auto()
    S4 = auto()


"""Define possible actions for the SDP"""


class Action(Enum):
    A1 = auto()
    A2 = auto()
    A3 = auto()
    A4 = auto()


"""Arbitrary implementation of the SDP class for testing"""


class TestSDP(SDP):

    @property
    def states(self):
        return list(State)

    """Define action behavior based on state, arbitrary."""

    def actions(self, t: int, x: State) -> list[Action] | list[None]:
        if x in [State.S1, State.S2]:
            return [Action.A1, Action.A2]
        elif x in [State.S3, State.S4]:
            return [Action.A3, Action.A4]

    """ 
    Arbitraily implemented next function. 
    The property tests make no assertions on the static values, only on the behaviour of the function.
    """

    def nextFunc(self, t: int, x: State, y: Action) -> dict[State, float]:
        if t == 0:
            if x == State.S1 or x == State.S2:
                if y == Action.A1:
                    return self.mkSimpleProb([(State.S1, 0.6), (State.S2, 0.4)])
                elif y == Action.A2:
                    return self.mkSimpleProb([(State.S1, 0.5), (State.S2, 0.5)])
                elif y == Action.A3:
                    return self.mkSimpleProb([(State.S3, 0.5), (State.S2, 0.5)])
                elif y == Action.A4:
                    return self.mkSimpleProb([(State.S1, 0.25), (State.S2, 0.75)])
            elif x == State.S3 or x == State.S4:
                if y == Action.A1:
                    return self.mkSimpleProb([(State.S3, 0.9), (State.S4, 0.1)])
                if y == Action.A3 or y == Action.A2:
                    return self.mkSimpleProb([(State.S3, 0.4), (State.S4, 0.6)])
                elif y == Action.A4:
                    return self.mkSimpleProb([(State.S3, 0.7), (State.S4, 0.3)])
        elif t > 0:
            if x == State.S1 or x == State.S2:
                if y == Action.A1:
                    return self.mkSimpleProb([(State.S1, 0.7), (State.S2, 0.3)])
                elif y == Action.A2:
                    return self.mkSimpleProb([(State.S1, 0.6), (State.S2, 0.4)])
                elif y == Action.A3:
                    return self.mkSimpleProb([(State.S1, 0.8), (State.S4, 0.2)])
                elif y == Action.A4:
                    return self.mkSimpleProb([(State.S2, 0.76), (State.S4, 0.24)])
            elif x == State.S3 or x == State.S4:
                if y == Action.A1:
                    return self.mkSimpleProb([(State.S2, 0.3), (State.S1, 0.7)])
                elif y == Action.A2:
                    return self.mkSimpleProb([(State.S3, 0.45), (State.S1, 0.55)])
                elif y == Action.A3:
                    return self.mkSimpleProb([(State.S3, 0.6), (State.S4, 0.4)])
                elif y == Action.A4:
                    return self.mkSimpleProb([(State.S3, 0.5), (State.S4, 0.5)])

    def reward(self, t: int, x: State, y: Action, next_x: State) -> float:
        if next_x in [State.S1, State.S2]:
            return 1.0
        else:
            return 0.0


sdp_instance = TestSDP()


# ==================== Property Tests: action  ====================


# Tests if the actions function always returns a list, t is irrelevant.
@given(st.integers(min_value=0), st.sampled_from(sdp_instance.states))
def test_actions_return_list(t, state):  # `t` is provided by Hypothesis
    actions = sdp_instance.actions(t, state)
    assert isinstance(actions, list)


# Tests that the elements in the list are of type Action
@given(st.integers(min_value=0), st.sampled_from(sdp_instance.states))
def test_actions_return_type_action(t, state):
    actions = sdp_instance.actions(t, state)
    assert (all(isinstance(a, Action)) or a is None for a in actions)


# Test that the function returns correct behaviour for a specifically given state
@given(st.integers(min_value=0), st.sampled_from(sdp_instance.states))
def test_actions_returned_valid(t, state):
    actions = sdp_instance.actions(t, state)
    if state in [State.S1, State.S2]:
        assert all(action in [Action.A1, Action.A2] for action in actions)


# ==================== Property Tests: nextFunc  ====================


# Test that nextFunc gives back a dictionary and that the dictionary if of type "dict[State, float]"
@given(
    st.integers(min_value=0),
    st.sampled_from(sdp_instance.states),
    st.sampled_from(list(Action)),
)
def test_nextFunc_return_dict(t, x, y):
    next = sdp_instance.nextFunc(t, x, y)
    assert isinstance(next, dict)
    assert all(
        isinstance(k, State) for k in next.keys()
    ), f"Keys are not all State instances: {next.keys()}"
    assert all(
        isinstance(v, float) for v in next.values()
    ), f"Values are not all float: {next.values()}"


# Test that Probabilities given from next always sum to 1
@given(
    st.integers(min_value=0),
    st.sampled_from(sdp_instance.states),
    st.sampled_from(list(Action)),
)
def test_nextFunc_probabilities_sum_1(t, x, y):
    next = sdp_instance.nextFunc(t, x, y)
    total = sum(next.values())
    assert abs(total - 1.0) < 1e-7


# Test that all probabilities are larger than 0
@given(
    st.integers(min_value=0),
    st.sampled_from(sdp_instance.states),
    st.sampled_from(list(Action)),
)
def test_nextFunc_no_negative_probs(t, x, y):
    next = sdp_instance.nextFunc(t, x, y)
    assert all(v >= 0 for v in next.values())


# Test that all states returned from the next function are legitimate states (member of the enum class State)
@given(
    st.integers(min_value=0),
    st.sampled_from(sdp_instance.states),
    st.sampled_from(list(Action)),
)
def test_nextFunc_valid_states(t, x, y):
    next = sdp_instance.nextFunc(t, x, y)
    assert set(next.keys()).issubset(set(sdp_instance.states))


# Test that we always get the same dictionary (same states and probabilities) given the exact same input.
@given(
    st.integers(min_value=0),
    st.sampled_from(sdp_instance.states),
    st.sampled_from(list(Action)),
)
def test_nextFunc_determinism(t, x, y):
    result1 = sdp_instance.nextFunc(t, x, y)
    result2 = sdp_instance.nextFunc(t, x, y)

    assert result1 == result2, f"Function is non-deterministic: {result1} != {result2}"


# ==================== Property Tests: reward  ====================


# Test to ensure reward always returns an integer
@given(
    st.integers(min_value=0),
    st.sampled_from(sdp_instance.states),
    st.sampled_from(sdp_instance.states),
    st.sampled_from(sdp_instance.states),
)
def test_reward_return_int(t: int, x: State, y: State, next_x: State):
    result = sdp_instance.reward(t, x, y, next_x)
    assert isinstance(result, float)


# Test that reward always returns a positive integer (no multiplication with 0)
@given(
    st.integers(min_value=0),
    st.sampled_from(sdp_instance.states),
    st.sampled_from(sdp_instance.states),
    st.sampled_from(sdp_instance.states),
)
def test_reward_return_positive_int(t: int, x: State, y: State, next_x: State):
    result = sdp_instance.reward(t, x, y, next_x)
    assert result >= 0


# Test that reward always gives the same reward given the same inputs:
@given(
    st.integers(min_value=0),
    st.sampled_from(sdp_instance.states),
    st.sampled_from(sdp_instance.states),
    st.sampled_from(sdp_instance.states),
)
def test_reward_stochastic(t: int, x: State, y: State, next_x: State):
    result1 = sdp_instance.reward(t, x, y, next_x)
    result2 = sdp_instance.reward(t, x, y, next_x)
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
        ps[0][nx] = Action.A1  # Assign a default action
    result = sdp_instance.val(t, ps, nx)
    assert isinstance(result, float)  """

# ==================== Property Tests: bestExt  ====================

#    def bestExt(self, t: int, ps_tail: list[dict[State, Action]]) -> dict[State, Action]:


# Test that bestExt returns a policy (dict[State, Action])

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
    )
)
def test_bestExt_return_value(t:int, tail: list[dict[State,Action]]):
    result = sdp_instance.bestExt(t, tail)
    assert(isinstance(result, dict))  """

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
    result_1 = sdp_instance.best(t=0, n=1, x=State.S1)
    result_2 = sdp_instance.best(t=0, n=2, x=State.S1)

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
    prev_value = sdp_instance.mMeas(t, 10, x)
    for n in range(6, 0, -1):
        curr_value = sdp_instance.mMeas(t, n, x)
        assert curr_value <= prev_value or math.isclose(
            curr_value, prev_value, abs_tol=1e-10
        ), f"mMeas({t}, {n}, {x}) increased unexpectedly: {prev_value} → {curr_value}"
