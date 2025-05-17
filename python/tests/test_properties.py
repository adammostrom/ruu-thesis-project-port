import re

import numpy as np
import pytest
from hypothesis import given
from hypothesis import strategies as st

"""
Testing the property of the SDP using pythons "hypothesis".
This test file aims to test and assure the properties of the functions defined by SDP 
WITHOUT MEMOIZATION.

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

 # Can be switched with the other commented SDP:s, as long as they in turn use the SDP framework WITHOUT memoization.
from src.implementations.MatterMostSDP import MatterMost as module

sdp_instance = module()

#from src.implementations.numberLineSDP import NumberLine as module
# sdp_instance = module()

# ==================== Property Tests: states ====================


# Tests if the actions function always returns a list, t is irrelevant.
@given(st.integers(min_value=0, max_value=10))
def test_states_return_list(t):  # `t` is provided by Hypothesis
    states = sdp_instance.states(t)
    assert isinstance(states, list)


# Test that the function will behave the same for same input
@given(st.integers(min_value=0, max_value=10))
def test_states_deterministic(t):
    result1 = sdp_instance.states(t)
    result2 = sdp_instance.states(t)
    assert result1 == result2


# Test that the `safe_states` method raises appropriate errors for invalid inputs.
def test_states_error_raised():
    # Invalid time step `t`
    with pytest.raises(ValueError):
        sdp_instance.safe_states(1.5)
    with pytest.raises(ValueError):
        sdp_instance.safe_states(-1)


# ==================== Property Tests: actions ====================


# Tests if the actions function always returns a list.
@given(st.integers(min_value=0, max_value=10))
def test_actions_return_list(t):  # `t` is provided by Hypothesis
    states = sdp_instance.states(t)
    for x in states:
        actions = sdp_instance.actions(t, x)
        assert isinstance(actions, list)


# Test that the function will behave the same for same input
@given(st.integers(min_value=0, max_value=10))
def test_actions_deterministic(t):
    states = sdp_instance.states(t)
    for x in states:
        result1 = sdp_instance.actions(t, x)
        result2 = sdp_instance.actions(t, x)
        assert result1 == result2


# Test that the `safe_actions` method raises appropriate errors for invalid inputs.
def test_actions_error_raised():
    x = sdp_instance.states(0)
    # Invalid time step `t`
    with pytest.raises(ValueError):
        sdp_instance.safe_actions(1.5, x)
    with pytest.raises(ValueError):
        sdp_instance.safe_actions(-1, x)
    # Invalid state `x`
    with pytest.raises(ValueError):
        sdp_instance.safe_actions(1, "InvalidState")


# ==================== Property Tests: nextFunc ====================


# Test that nextFunc gives back a dictionary and that the dictionary if of type "dict[State, float]"
@given(st.integers(min_value=1, max_value=10))
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
    st.integers(min_value=1, max_value=10))
def test_nextFunc_probabilities_sum_1(t):
    states = sdp_instance.states(t)
    for x in states:
        y = sdp_instance.actions(t, x)
        next = sdp_instance.nextFunc(t, x, y[0])
        total = sum(next.values())
        assert abs(total - 1.0) < 1e-7


# Test that all probabilities are larger than 0
@given(
    st.integers(min_value=0, max_value=10))
def test_nextFunc_no_negative_probs(t):
    states = sdp_instance.states(t)
    for x in states:
        y = sdp_instance.actions(t, x)
        next = sdp_instance.nextFunc(t, x, y[0])
        assert all(v >= 0 for v in next.values())


# Test that all states returned from the next function when executed at time t 
# are legitimate at time t+1.
@given(st.integers(min_value=0, max_value=10))
def test_nextFunc_valid_states(t):
    current_states = sdp_instance.states(t)
    next_states = sdp_instance.states(t+1)
    for x in current_states:
        y = sdp_instance.actions(t, x)
        next = sdp_instance.nextFunc(t, x, y[0])
        assert set(next.keys()).issubset(set(next_states))


# Test that we always get the same dictionary (same states and probabilities) given the exact same input.
@given(st.integers(min_value=0, max_value=10))
def test_nextFunc_determinism(t):
    states = sdp_instance.states(t)
    for x in states:
        y = sdp_instance.actions(t, x)
        result1 = sdp_instance.nextFunc(t, x, y[0])
        result2 = sdp_instance.nextFunc(t, x, y[0])
        assert result1 == result2, f"Function is non-deterministic: {result1} != {result2}"


# Test that the `safe_nextFunc` method raises appropriate errors for invalid inputs.
def test_nextFunc_error_raised():
    x = sdp_instance.states(0)[0]
    y = sdp_instance.actions(0, x)[0]
    # Invalid time step `t`
    with pytest.raises(ValueError):
        sdp_instance.safe_nextFunc(1.5, x, y)
    with pytest.raises(ValueError):
        sdp_instance.safe_nextFunc(-1, x, y)
    # Invalid state `x`
    with pytest.raises(ValueError):
        sdp_instance.safe_nextFunc(1, "InvalidState", y)
    # Invalid action `y`
    with pytest.raises(ValueError):
        sdp_instance.safe_nextFunc(1, x, "InvalidAction")


# ==================== Property Tests: reward  ====================


# Test that reward always gives the same reward given the same inputs:
@given(st.integers(min_value=1, max_value=10))
def test_reward_stochastic(t: int):
    states = sdp_instance.states(t)
    for x in states:
        for next_x in states:         
            y = sdp_instance.actions(t, x)
            result1 = sdp_instance.reward(t, x, y[0], next_x)
            result2 = sdp_instance.reward(t, x, y[0], next_x)
            assert result1 == result2


# Test that the `safe_reward` method raises appropriate errors for invalid inputs.
def test_reward_error_raised():
    x = sdp_instance.states(0)[0]
    y = sdp_instance.actions(0, x)[0]
    x_prim = x
    # Invalid time step `t`
    with pytest.raises(ValueError):
        sdp_instance.safe_reward(1.5, x, y, x_prim)
    with pytest.raises(ValueError):
        sdp_instance.safe_reward(-1, x, y, x_prim)
    # Invalid state `x`
    with pytest.raises(ValueError):
        sdp_instance.safe_reward(1, "InvalidState", y, x_prim)
    # Invalid action `y`
    with pytest.raises(ValueError):
        sdp_instance.safe_reward(1, x, "InvalidAction", x_prim)
    # Invalid next state `next_x`
    with pytest.raises(ValueError):
        sdp_instance.safe_reward(0, x, y, "InvalidState")


# ==================== Property Tests: mkSimpleProb ====================


# Test: Valid Probabilities Sum to 1. Function uses the instance only to 
# apply a type x, the real test just checks that mkSimpleProb 
# given a list of tuples return true of the sum of probabilites are 1.
def test_mkSimpleProb_valid():
    states = sdp_instance.states(0)
    x1 = states[0]
    x2 = states[1]
    test_data = [(x1, 0.4), (x2, 0.6)]
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


# ==================== Property Tests: add ====================


# Test that add always returns a plus the discounted b.
@given(
    st.floats(min_value=-2, max_value=2),
    st.floats(min_value=-2, max_value=2)
)
def test_add_valid_sum(a, b):
    assert sdp_instance.add(a, b) == a + b * sdp_instance.discountRate


# Test: Add function rejects invalid inputs.
def test_add_invalid():
    with pytest.raises(TypeError, match=f"Inputs must be of type 'float', not 'int' and 'int'."):
        sdp_instance.add(1, 1)


# ==================== Property Tests: meas ====================


# Test that expected value is returned.
@given(
    st.floats(min_value=-2, max_value=2),
    st.floats(min_value=-2, max_value=2),
    st.floats(min_value=-2, max_value=2),
    st.floats(min_value=-2, max_value=2),
)
def test_meas_valid(a, p1, b, p2):
    test_data = [(a, p1), (b, p2)]
    result = sdp_instance.meas(test_data)
    assert result == a * p1 + b * p2


# Test that the return of meas is a float.
def test_meas_invalid():
    with pytest.raises(TypeError, match="Input must be a list of tuples of floats."):
        sdp_instance.meas([(0, 0)])


# ==================== Property Tests: val ====================


# Test that the return of val is a float
@given(
    st.integers(min_value=0, max_value=5)
)
def test_val_return_type(t: int):
    states = sdp_instance.states(t)
    for x in states:
        ps = [ {s: sdp_instance.actions(t, s)[0] for s in states} ]
        result = sdp_instance.val(t, ps, x)
        assert isinstance(result, float)


# Test that the val function returns the same value for equal inputs.
@given(st.integers(min_value=0, max_value=8))
def test_val_deterministic(t):
    states = sdp_instance.states(t)
    for x in states:
        ps = [ {s: sdp_instance.actions(t, s)[0] for s in states} ]
        val1 = sdp_instance.val(t, ps, x)
        val2 = sdp_instance.val(t, ps, x)
        assert val1 == val2


# Test that the val function returns zero value when given empty policy sequence as input.
@given(st.integers(min_value=0, max_value=8))
def test_val_with_empty_policy_list(t):
    for x in sdp_instance.states(t):
        result = sdp_instance.val(t, [], x)
        assert result == sdp_instance.zero


# Test: Val function rejects invalid time input. 
@given(st.integers(min_value=0, max_value=8))
def test_val_with_invalid_time_step(t):
    for x in sdp_instance.states(t):
        with pytest.raises(ValueError):
            sdp_instance.val(-1, [], x)
            sdp_instance.val(1.5, [], x)


# Test: Val function rejects invalid state input. 
def test_val_with_invalid_state():
    with pytest.raises(ValueError):
        sdp_instance.val(0, [], "Invalid")


# Test that val function returns zero value if given state is not in first policy
# of given policy sequence.
def test_val_with_state_not_in_policy():
    states = sdp_instance.states(0)
    x1 = states[0]
    x2 = states[1]
    actions = sdp_instance.actions(0, x1)
    for y in actions:
        ps = [{x1: y}]
        result = sdp_instance.val(0, ps, x2)
        assert result == sdp_instance.zero


# ==================== Property Tests: bestExt ====================


# Test that bestExt returns a policy (dict[State, Action])
@given(st.integers(min_value=0, max_value=5))
def test_bestExt_return_value(t: int):
    states = sdp_instance.states(t+1)
    ps_tail = [ {s: sdp_instance.actions(t+1, s)[0] for s in states} ]
    result = sdp_instance.bestExt(t, ps_tail)
    assert isinstance(result, dict)
    assert ps_tail


# The output policy should only assign valid actions to each state.
# That means policy[state] should be in self.actions(t, state).
@given(st.integers(min_value=0, max_value=5))
def test_bestExt_valid_actions(t: int):
    states = sdp_instance.states(t+1)
    ps_tail = [ {s: sdp_instance.actions(t+1, s)[0] for s in states} ]
    policy = sdp_instance.bestExt(t, ps_tail)
    for state, action in policy.items():
        assert action in sdp_instance.actions(t, state)


# Test that running bestExt with the same inputs should return the same policy
@given(st.integers(min_value=0, max_value=5))
def test_bestExt_deterministic(t):
    states = sdp_instance.states(t+1)
    ps_tail = [ {s: sdp_instance.actions(t+1, s)[0] for s in states} ]
    result1 = sdp_instance.bestExt(t, ps_tail)
    result2 = sdp_instance.bestExt(t, ps_tail)
    assert result1 == result2


# Test that the actions from the best policy gives a higher value than any other.
@given(st.integers(min_value=0, max_value=100))
def test_bestExt_optimality(t):
    states = sdp_instance.states(t+1)
    ps_tail = [ {s: sdp_instance.actions(t+1, s)[0] for s in states} ]
    policy = sdp_instance.bestExt(t, ps_tail)
    for state, action in policy.items():
        best_value = sdp_instance.val(t, [{state: action}] + ps_tail, state)
        for other_action in sdp_instance.actions(t, state):
            value = sdp_instance.val(t, [{state: other_action}] + ps_tail, state)
            assert value <= best_value


# ==================== Property Tests: worstExt ====================


# Test that worstExt returns a policy (dict[State, Action])
@given(st.integers(min_value=0, max_value=5))
def test_worstExt_return_value(t):
    states = sdp_instance.states(t+1)
    ps_tail = [ {s: sdp_instance.actions(t+1, s)[0] for s in states} ]
    result = sdp_instance.worstExt(t, ps_tail)
    assert isinstance(result, dict)
    assert ps_tail


# The output policy should only assign valid actions to each state.
# That means policy[state] should be in self.actions(t, state).
@given(st.integers(min_value=0, max_value=5)) 
def test_worstExt_valid_actions(t):
    states = sdp_instance.states(t+1)
    ps_tail = [ {s: sdp_instance.actions(t+1, s)[0] for s in states} ]
    policy = sdp_instance.worstExt(t, ps_tail)
    for state, action in policy.items():
        assert action in sdp_instance.actions(t, state)


# Test that running worstExt with the same inputs should return the same policy
@given(st.integers(min_value=0, max_value=5))
def test_worstExt_deterministic(t):
    states = sdp_instance.states(t+1)
    ps_tail = [ {s: sdp_instance.actions(t+1, s)[0] for s in states} ]
    result1 = sdp_instance.bestExt(t, ps_tail)
    result2 = sdp_instance.bestExt(t, ps_tail)
    assert result1 == result2


# Test that the actions from the worst policy gives a lower value than any other.
@given(st.integers(min_value=0, max_value=10))
def test_worstExt_suboptimality(t):
    states = sdp_instance.states(t+1)
    ps_tail = [ {s: sdp_instance.actions(t+1, s)[0] for s in states} ]
    policy = sdp_instance.worstExt(t, ps_tail)
    for state, action in policy.items():
        worst_value = sdp_instance.val(t, [{state: action}] + ps_tail, state)
        for other_action in sdp_instance.actions(t, state):
            value = sdp_instance.val(t, [{state: other_action}] + ps_tail, state)
            assert value >= worst_value


# ==================== Property Tests: randomExt ====================


# Test that randomExt returns a policy (dict[State, Action])
@given(st.integers(min_value=0, max_value=5))
def test_randomExt_return_value(t):
    states = sdp_instance.states(t+1)
    ps_tail = [ {s: sdp_instance.actions(t+1, s)[0] for s in states} ]
    result = sdp_instance.randomExt(t, ps_tail)
    assert isinstance(result, dict)
    assert ps_tail


# The output policy should only assign valid actions to each state.
# That means policy[state] should be in self.actions(t, state).
@given(st.integers(min_value=0, max_value=5)) 
def test_randomExt_valid_actions(t):
    states = sdp_instance.states(t+1)
    ps_tail = [ {s: sdp_instance.actions(t+1, s)[0] for s in states} ]
    policy = sdp_instance.randomExt(t, ps_tail)
    for state, action in policy.items():
        assert action in sdp_instance.actions(t, state)


# ==================== Property Tests: bi ====================


# Test that the length of the computed sequence steps is equal to the given limit/horizon.
@given(st.integers(min_value=0, max_value=5), st.integers(min_value=0, max_value=3))
def test_bi_length(t, n):
    result = sdp_instance.bi(t, n)
    assert len(result) == n


# Test that the bi function upholds its recursive structure, by checking if the next value 
# in the "stack" is different from the previous one.
@given(st.integers(min_value=1, max_value=5), st.integers(min_value=1, max_value=4))
def test_bi_recursive_structure_2(t, n):
    result = sdp_instance.bi(t, n)
    result2 = sdp_instance.bi(t+1, n-1)
    assert result[1:] == result2


# Test that the return values are of correct type (State, Action).
@given(st.integers(min_value=0, max_value=5), st.integers(min_value=0, max_value=3))
def test_bi_valid_returns(t, n):
    result = sdp_instance.bi(t, n)
    for p in result:
        for state, action in p.items():
            assert state in sdp_instance.states(t)
            assert action in sdp_instance.actions(t, state)  # Ensure valid action
            t += 1


# Test that that the optimal policy is the same given same input type.
@given(st.integers(min_value=0, max_value=5), st.integers(min_value=0, max_value=3))
def test_bi_consistency(t, n):
    result1 = sdp_instance.bi(t, n)
    result2 = sdp_instance.bi(t, n)
    assert result1 == result2


# Test that the base case returns an empty list.
@given(st.integers(min_value=0, max_value=5))
def test_bi_base_case(t):
    result = sdp_instance.bi(t, 0)
    assert result == []


# Test that a sequence of length 1 is equal to an optimal extension.
def test_bi_with_n_one():
    ps_best = sdp_instance.bestExt(0, [])
    result = sdp_instance.bi(0, 1)
    assert result == [ps_best]


# Test that a sequence of length 2 is equal to two optimal extensions, the first
# one building on the second.
def test_bi_with_n_two():
    ps_best_1 = sdp_instance.bestExt(1, [])
    ps_best_0 = sdp_instance.bestExt(0, [ps_best_1])
    result = sdp_instance.bi(0, 2)
    assert result == [ps_best_0, ps_best_1]


# Test that invalid time step raises error.
def test_bi_with_invalid_time_step():
    with pytest.raises(ValueError):
        sdp_instance.bi(-1, 1)


# ==================== Property Tests: randomPS ====================


# Test that the length of the computed sequence steps is equal to the given limit/horizon.
@given(st.integers(min_value=0, max_value=5), st.integers(min_value=0, max_value=5))
def test_randomPS_length(t, n):
    result = sdp_instance.randomPS(t, n)
    assert len(result) == n


# Test that the return values are of correct type (State, Action).
@given(st.integers(min_value=0, max_value=5), st.integers(min_value=0, max_value=5))
def test_randomPS_valid_returns(t, n):
    result = sdp_instance.randomPS(t, n)
    for p in result:
        for state, action in p.items():
            assert state in sdp_instance.states(t)
            assert action in sdp_instance.actions(t, state)  # Ensure valid action
            t += 1

# Test that the base case returns an empty list.
@given(st.integers(min_value=0, max_value=5))
def test_bi_base_case(t):
    result = sdp_instance.randomPS(t, 0)
    assert result == []


# Test that invalid time step raises error.
def test_randomPS_with_invalid_time_step():
    with pytest.raises(ValueError):
        sdp_instance.randomPS(-1, 1)


# ==================== Property Tests: best ====================


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
    extended_policy = sdp_instance.bestExt(0, ps)
    result = sdp_instance.best(0, 3, x)
    action = extended_policy[x]
    val = sdp_instance.val(0, [extended_policy] + ps, x)
    assert action.name in result
    assert f"{np.round(val, 10)}"[:-1] in result


# Test that makes sure an increase in horizon changes the result provided by the best function. 
# This test specifically focuses on the string return behavour.
def test_best_horizon_increasing():
    states = sdp_instance.states(0)
    result_1 = sdp_instance.best(0, 1, states[0])
    result_2 = sdp_instance.best(0, 2, states[0])
    assert result_1 != result_2


# ==================== Property Tests: worst ====================


# Test that the function returns a correct error response to a horizon < 1
def test_worst_invalid_horizon():
    states = sdp_instance.states(0)
    x = states[0]
    with pytest.raises(ValueError):
        sdp_instance.worst(0, 0, x)


# Test that the return string matches the format: "Horizon, worst, value : {n}, {b}, {vb}"
def test_worst_output_format():
    states = sdp_instance.states(0)
    x = states[0]
    result = sdp_instance.worst(0, 2, x)
    assert re.match(
        r"Horizon, worst, value : \d+, .*?, .*?", result
    ), f"Unexpected output format: {result}"


# Test that given a policy sequence, the extended sequence should include the subset of the previous sequence.
# Example: sequence = [1,2,3], extended sequence = [1,2,3,4,5], the extended sequence contains the subset "[1,2,3]"
def test_worst_policy_consistency():
    states = sdp_instance.states(0)
    x = states[0]
    ps = sdp_instance.bi(1, 2)
    extended_policy = sdp_instance.worstExt(0, ps)
    result = sdp_instance.worst(0, 3, x)
    action = extended_policy[x]
    val = sdp_instance.val(0, [extended_policy] + ps, x)
    assert action.name in result
    assert f"{np.round(val, 8)}"[:-1] in result


# Test that makes sure an increase in horizon changes the result provided by the best function.
# This test specifically focuses on the string return behavour.
def test_worst_horizon_increasing():
    states = sdp_instance.states(0)
    result_1 = sdp_instance.worst(0, 1, states[0])
    result_2 = sdp_instance.worst(0, 2, states[0])
    assert result_1 != result_2


# ==================== Property Tests: mMeas ====================


# Test that the return type of mMeas is correct.
@given(
    st.integers(min_value=0, max_value=5),
    st.integers(min_value=1, max_value=3),
)
def test_mMeas_output_type(t, n):
    states = sdp_instance.states(t)
    x = states[0]
    result = sdp_instance.mMeas(t, n, x)
    assert isinstance(result, float)


# Test that the function always returns a value between 0 and 1
@given(
    st.integers(min_value=0, max_value=5),
    st.integers(min_value=1, max_value=3),
)
def test_mMeas_output_range(t, n):
    states = sdp_instance.states(t)
    x = states[0]
    result = sdp_instance.mMeas(t, n, x)
    assert 0 <= result <= 1, f"mMeas({t}, {n}, {x}) = {result}, expected in [0,1]"