import math
import re
from enum import Enum, auto

import pytest
from hypothesis import given
from hypothesis import strategies as st

"""
Testing the property of the SDP using pythons "hypothesis".
This test file aims to test and assure the properties of the functions defined by SDP.

Python Hypothesis 
- @given(...)                       = Hypothesis decorator for automatically generates test cases.
- st.integers(min_value=0)          = Randomly generates integer time steps (t).
- st.sampled_from(MySDP().states()) = Randomly picks a state from the list of valid states.

"""
############################## HOW TO RUN ######################################
# Requires: pytest (pip install pytest)
#
# Import relevant SDP to test in the "Test SDP Implementation" section as "module"
#
# navigate to:
# > ruu-thesis-project-port/ruu-thesis-project-port/python
#
# Start Python Shell:
# > python3
#
# Run:
# > pytest -s tests/test_properties.py -v
###############################################################################

# ==================== Test SDP Implementation ====================

from src.implementations.MatterMostMemo import MatterMost as module

#from src.implementations.MatterMostSDP import MatterMost as module

sdp_instance = module()


# ==================== Property Tests: action  ====================


# Tests if the actions function always returns a list, t is irrelevant.
@given(st.integers(min_value=0, max_value=8))
def test_actions_return_list(t):  # `t` is provided by Hypothesis
    
    states = sdp_instance.states(t)
    for x in states:
        actions = sdp_instance.actions(t, x)
        assert isinstance(actions, list)


# Test that the function will behave the same for same input
@given(st.integers(min_value=0))
def test_actions_deterministic(t):
    states = sdp_instance.states(t)
    for x in states:
        result1 = sdp_instance.actions(t, x)
        result2 = sdp_instance.actions(t, x)
        assert result1 == result2


# ==================== Property Tests: nextFunc  ====================

# Test that nextFunc gives back a dictionary and that the dictionary if of type "dict[State, float]"
@given(st.integers(min_value=1, max_value=8))
def test_nextFunc_return_dict(t):
    states = sdp_instance.states(t)
    for x in states:
        y = sdp_instance.actions(t, x)
        next = sdp_instance.nextFunc(t, x, y[0])
        assert isinstance(next, dict)
        assert all(isinstance(k, type(x)) for k in next.keys())
        assert all(isinstance(v, float) for v in next.values())


# Test that Probabilities given from next always sum to 1
@given(
    st.integers(min_value=1, max_value=8))
def test_nextFunc_probabilities_sum_1(t):
    states = sdp_instance.states(t)
    for x in states:
        y = sdp_instance.actions(t, x)
        next = sdp_instance.nextFunc(t, x, y[0])
        total = sum(next.values())
        assert abs(total - 1.0) < 1e-7


# Test that all probabilities are larger than 0
@given(
    st.integers(min_value=0, max_value=8))
def test_nextFunc_no_negative_probs(t):
    states = sdp_instance.states(t)
    for x in states:
        y = sdp_instance.actions(t, x)
        next = sdp_instance.nextFunc(t, x, y[0])
        assert all(v >= 0 for v in next.values())


# Test that all states returned from the next function are legitimate states (member of the enum class State)
@given(
    st.integers(min_value=0, max_value=8))
def test_nextFunc_valid_states(t):
    states = sdp_instance.states(t)
    for x in states:
        y = sdp_instance.actions(t, x)
        next = sdp_instance.nextFunc(t, x, y[0])
        assert set(next.keys()).issubset(set(states))

# Test that we always get the same dictionary (same states and probabilities) given the exact same input.
@given(
    st.integers(min_value=0, max_value=8)
)
def test_nextFunc_determinism(t):
    states = sdp_instance.states(t)
    for x in states:
        y = sdp_instance.actions(t, x)
        result1 = sdp_instance.nextFunc(t, x, y[0])
        result2 = sdp_instance.nextFunc(t, x, y[0])
        assert result1 == result2, f"Function is non-deterministic: {result1} != {result2}"



# ==================== Property Tests: reward  ====================


# Test that reward always returns a positive integer (no multiplication with 0)
@given(
    st.integers(min_value=0, max_value=8)
)
def test_reward_return_positive_int(t: int):
    states = sdp_instance.states(t)
    for x in states:
        for next_x in states:
            y = sdp_instance.actions(t, x)
            result = sdp_instance.reward(t, x, y[0], next_x)
            assert result >= 0
    
    


# Test that reward always gives the same reward given the same inputs:
@given(
    st.integers(min_value=1, max_value=8))
def test_reward_stochastic(t: int):
    states = sdp_instance.states(t)
    for x in states:
        for next_x in states:         
            y = sdp_instance.actions(t, x)
            result1 = sdp_instance.reward(t, x, y[0], next_x)
            result2 = sdp_instance.reward(t, x, y[0], next_x)
            assert result1 == result2



# ==================== Property Tests: mkSimpleProb  ====================


# Test: Valid Probabilities Sum to 1. Function uses the instance only to apply a type x and a type y, the real test just checks that mkSimpleProb given a list of tuples return true of the sum of probabilites are 1.
def test_mkSimpleProb_valid():
    states = sdp_instance.states(0)
    x = states[0]
    next_x = states[1]
    test_data = [(x, 0.4), (next_x, 0.6)]
    result = sdp_instance.mkSimpleProb(test_data)
    assert sum(result.values()) == 1.0


# Test: Rejects Negative Probabilities
def test_mkSimpleProb_no_negative_probs():
    states = sdp_instance.states(0)
    x = states[0]
    next_x = states[1]
    test_data = [(x, -0.1), (next_x, 1.1)]
    with pytest.raises(ValueError, match="No negative probabilities allowed."):
        sdp_instance.mkSimpleProb(test_data)


# Test: Rejects Probabilities That Do Not Sum to 1
def test_mkSimpleProb_invalid_sum():
    states = sdp_instance.states(0)
    x = states[0]
    next_x = states[1]
    test_data = [(x, 0.3), (next_x, 0.3)]
    with pytest.raises(ValueError, match="Probabilities do not sum to 1"):
        sdp_instance.mkSimpleProb(test_data)


# ==================== Property Tests: val  ====================


# Test that the return of val is a float
@given(
    st.integers(min_value=0, max_value=5)
)
def test_val_return(t: int):
    states = sdp_instance.states(t)
    for x in states:
        for nx in states:
            ps = [ {s: (sdp_instance.actions(t, s)[0], None) for s in states} ]
            result = sdp_instance.val(t, ps, nx)
            assert isinstance(result, float)


# Test that the val function returns the same value for equal inputs.
@given(
    st.integers(min_value=0, max_value=10))
def test_val_deterministic(t):
    states = sdp_instance.states(t)
    for x in states:
        for nx in states:
            action = sdp_instance.actions(t, x)
            ps = [ {s: (sdp_instance.actions(t, s)[0], None) for s in states} ]

            val1 = sdp_instance.val(t, ps, nx)
            val2 = sdp_instance.val(t, ps, nx)
            assert val1 == val2



# ==================== Property Tests: bestExt  ====================


# Test that bestExt returns a policy (dict[State, Action])
@given(st.integers(min_value=0, max_value=5))
def test_bestExt_return_value(t: int):
    states = sdp_instance.states(t)
    ps_tail = [ {s: (sdp_instance.actions(t, s)[0], None) for s in states} ]

    result = sdp_instance.bestExt(t, ps_tail)
    assert isinstance(result, dict)
    assert ps_tail


# The output policy should only assign valid actions to each state.
# That means policy[state] should be in self.actions(t, state).
@given(st.integers(min_value=0, max_value=5))
def test_bestExt_valid_actions(t: int):

    states = sdp_instance.states(t)
    ps_tail = [ {s: (sdp_instance.actions(t, s)[0], None) for s in states} ]

    policy = sdp_instance.bestExt(t, ps_tail)
    for state, (action,_) in policy.items():
        assert action in sdp_instance.actions(t, state)


# Test that running bestExt with the same inputs should return the same policy
@given(st.integers(min_value=0, max_value=5))
def test_bestExt_deterministic(t):
    states = sdp_instance.states(t)
    for x in states:
        ps_tail = [ {s: (sdp_instance.actions(t, s)[0], None) for s in states} ]

        result1 = sdp_instance.bestExt(t, ps_tail)
        result2 = sdp_instance.bestExt(t, ps_tail)
        assert result1 == result2


# This test tangents the "val" function, but bestExt should return the best (optimal) result given from val:
# If val(t, [p] + ps_tail, state) is strictly better for some action a', bestExt should return a'.
@given(st.integers(min_value=0, max_value=100))
def test_bestExt_optimality(t):
    states = sdp_instance.states(t)
    for x in states:
        action = sdp_instance.actions(t, x)
        ps_tail = [ {s: (sdp_instance.actions(t, s)[0], None) for s in states} ]

        policy = sdp_instance.bestExt(t, ps_tail)
        for state, (action,_) in policy.items():
            best_value = sdp_instance.val(t, [{state: (action, None)}] + ps_tail, state)

            for other_action in sdp_instance.actions(t, state):
                value = sdp_instance.val(t, [{state: (other_action, None)}] + ps_tail, state)
                assert value <= best_value


# Test that the function work consistently over different t values
@given(st.integers(min_value=0, max_value=10))
def test_bestExt_stability(t):
    states = sdp_instance.states(t)
    for x in states:
        ps_tail = [ {s: (sdp_instance.actions(t, s)[0], None) for s in states} ]

        policy1 = sdp_instance.bestExt(t, ps_tail)
        policy2 = sdp_instance.bestExt(t + 1, ps_tail)
        assert policy1.keys() == policy2.keys()


# ==================== Property Tests: worstExt  ====================


# That means policy[state] should be in self.actions(t, state).
@given(st.integers(min_value=0, max_value=5)) 
def test_worstExt_valid_actions(t: int):
    states = sdp_instance.states(t)
    for x in states:
        action = sdp_instance.actions(t, x)
        ps_tail = [ {s: (sdp_instance.actions(t, s)[0], None) for s in states} ]

        policy = sdp_instance.bestExt(t, ps_tail)
        for state, (action,_) in policy.items():
            assert action in sdp_instance.actions(t, state)


# Test that running worstExt with the same inputs should return the same policy
@given(st.integers(min_value=0, max_value=5))
def test_worstExt_deterministic(t):
    states = sdp_instance.states(t)
    for x in states:
        ps_tail = [ {s: (sdp_instance.actions(t, s)[0], None) for s in states} ]

        result1 = sdp_instance.bestExt(t, ps_tail)
        result2 = sdp_instance.bestExt(t, ps_tail)
        assert result1 == result2


# This test tangents the "val" function, but bestExt should return the best (optimal) result given from val:
# If val(t, [p] + ps_tail, state) is strictly better for some action a', bestExt should return a'.
@given(st.integers(min_value=0, max_value=100))
def test_worstExt_optimality(t):

    states = sdp_instance.states(t)
    for x in states:
        action = sdp_instance.actions(t, x)
        ps_tail = [ {s: (sdp_instance.actions(t, s)[0], None) for s in states} ]

        policy = sdp_instance.bestExt(t, ps_tail)
        for state, (action,_) in policy.items():
            best_value = sdp_instance.val(t, [{state: (action, None)}] + ps_tail, state)
            for other_action in sdp_instance.actions(t, state):
                value = sdp_instance.val(t, [{state: (other_action, None)}] + ps_tail, state)
                assert value <= best_value


# Test that the function work consistently over different t values
@given(st.integers(min_value=0, max_value=10))
def test_worstExt_stability(t):
    states = sdp_instance.states(t)
    for x in states:
        ps_tail = [ {s: (sdp_instance.actions(t, s)[0], None) for s in states} ]

        policy1 = sdp_instance.bestExt(t, ps_tail)
        policy2 = sdp_instance.bestExt(t + 1, ps_tail)
        assert policy1.keys() == policy2.keys()


# ==================== Property Tests: bi  ====================


# Test that the length of the computed sequence steps dont supersedes the given limit/horizon
@given(st.integers(min_value=0, max_value=5), st.integers(min_value=0, max_value=5))
def test_bi_length(t, n):
    result = sdp_instance.bi(t, n)
    assert len(result) == n


# Test that the bi function upholds its recursive structure, by checking if the next value in the "stack" is different from the previous one.
@given(st.integers(min_value=1, max_value=5), st.integers(min_value=0, max_value=5))
def test_bi_recursive_structure_2(t, n):
    result = sdp_instance.bi(t, 5)
    result2 = sdp_instance.bi(t, 4)
    assert result[1:] == result2


# Test that the return values are of correct type (State, Action)
@given(st.integers(min_value=0, max_value=5), st.integers(min_value=0, max_value=5))
def test_bi_valid_returns(t, n):
    result = sdp_instance.bi(t, n)
    for p in result:
        for state, (action,_)in p.items():
            assert state in sdp_instance.states(t)
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
    states = sdp_instance.states(0)
    x = states[0]
    with pytest.raises(ValueError):
        sdp_instance.best(0, 0, x)


# Test that the return string matches the format: "Horizon, best, value : {n}, {b}, {vb}"
def test_best_output_format():
    states = sdp_instance.states(0)
    x = states[0]
    result = sdp_instance.best(0, 2, x)
    assert re.match(
        r"Horizon, best, value : \d+, .*?, .*?", result
    ), f"Unexpected output format: {result}"


# Test that given a policy sequence, the extended sequence should include the subset of the previous sequence.
# Example: sequence = [1,2,3], extended sequence = [1,2,3,4,5], the extended sequence contains the subset "[1,2,3]"
def test_best_policy_consistency():
    states = sdp_instance.states(0)
    x = states[0]
    ps = sdp_instance.bi(1, 2)
    extended_policy = sdp_instance.bestExt(1, ps)
    result = sdp_instance.best(1, 3, x)
    action, val = extended_policy[x]
    assert action.name in result
    assert f"{val:.10f}"[:12] in result


# Test that makes sure an increase in horizon changes the result provided by the best function. This test specifically focuses on the string return behavour.
def test_best_horizon_increasing():
    states = sdp_instance.states(0)
    result_1 = sdp_instance.best(0, 1, states[0])
    result_2 = sdp_instance.best(0, 2, states[0])

    assert result_1 != result_2


# ==================== Property Tests: mMeas  ====================


# Test that the return type of mMeas is correct
# Test that the function always returns a value between 0 and 1
@given(
    st.integers(min_value=0, max_value=5),
    st.integers(min_value=1, max_value=5),
)
def test_mMeas_output_range(t, n):
    states = sdp_instance.states(t)
    x = states[0]
    result = sdp_instance.mMeas(t, n, x)
    assert isinstance(result, float)


# Test that the function always returns a value between 0 and 1
@given(
    st.integers(min_value=0, max_value=5),
    st.integers(min_value=1, max_value=5),
)
def test_mMeas_output_range(t, n):
    states = sdp_instance.states(t)
    x = states[0]
    result = sdp_instance.mMeas(t, n, x)
    assert 0 <= result <= 1, f"mMeas({t}, {n}, {x}) = {result}, expected in [0,1]"


def test_mMeas_monotonicity():
    states = sdp_instance.states(0)
    x = states[0]
    t = 1
    prev_value = sdp_instance.mMeas(t, 7, x)
    for n in range(6, 0, -1):
        curr_value = sdp_instance.mMeas(t, n, x)
        assert curr_value <= prev_value or math.isclose(
            curr_value, prev_value, abs_tol=1e-10
        ), f"mMeas({t}, {n}, {x}) increased unexpectedly: {prev_value} → {curr_value}"
