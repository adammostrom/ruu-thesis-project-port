import pytest
from hypothesis import given
from hypothesis import strategies as st
from src.implementations.numberLineSDP import Action, NumberLine, State

# Initialize an instance of the NumberLine class
nli  = NumberLine()

# -------------------------------------------------------------------------------
# Test: `states` method
# -------------------------------------------------------------------------------


# Test that the `states` method returns the correct list of states for given time step.
@given(st.integers(min_value=0, max_value=8))
def test_states_return(t: int):
    assert len(nli.states(t)) == 2*t+3


# -------------------------------------------------------------------------------
# Test: `actions` method
# -------------------------------------------------------------------------------


# Test that the `actions` method returns the correct list of actions for given states.
# Note: The time step `t` is currently irrelevant for the `actions` function.
@given(st.integers(min_value=0, max_value=8))
def test_actions_return(t: int):
    x1 = str(t-1)
    x2 = str(t)
    x3 = str(t+1)
    assert (nli.actions(t, x1)) == [Action.Left, Action.Right, Action.Stay]
    assert (nli.actions(t, x2)) == [Action.Left, Action.Right, Action.Stay]
    assert (nli.actions(t, x3)) == [Action.Left, Action.Right, Action.Stay]


# -------------------------------------------------------------------------------
# Test: `nextFunc` method
# -------------------------------------------------------------------------------


# Helper function to test_nextFunc_trivial
def assertDictAlmostEqual(predicted, expected):
    assert set(predicted.keys()) == set(expected.keys())
    for key in expected:
        assert abs(predicted[key] - expected[key]) < 1e-7


# Test specific cases for `nextFunc` where expected results are pre-calculated.
def test_nextFunc_trivial():
    assertDictAlmostEqual(nli.nextFunc(0, "0", Action.Left), {'-1': 0.85, '0': 0.1, '1': 0.05})
    assertDictAlmostEqual(nli.nextFunc(0, "0", Action.Stay), {'-1': 0.1, '0': 0.8, '1': 0.1})
    assertDictAlmostEqual(nli.nextFunc(0, "0", Action.Right), {'-1': 0.05, '0': 0.1, '1': 0.85})
    assertDictAlmostEqual(nli.nextFunc(1, "2", Action.Left), {'1': 0.85, '2': 0.1, '3': 0.05})
    assertDictAlmostEqual(nli.nextFunc(1, "2", Action.Stay), {'1': 0.1, '2': 0.80, '3': 0.1})
    assertDictAlmostEqual(nli.nextFunc(1, "2", Action.Right), {'1': 0.05, '2': 0.1, '3': 0.85})


# -------------------------------------------------------------------------------
# Test: `reward` method
# -------------------------------------------------------------------------------


# Test that the `reward` method returns the correct reward based on the next state.
@given(st.integers(min_value=0, max_value=8))
def test_reward_states(t):
    states = nli.states(t)
    for x in states:
        actions = nli.actions(t, x)
        for y in actions:
            next_xs = nli.nextFunc(t, x, y)
            for x_prim in next_xs:
                expected_reward = float(x_prim)
                assert nli.reward(t, x, y, x_prim) == expected_reward