import math
import pytest
from hypothesis import given
from hypothesis import strategies as st
from src.implementations.MatterMostSDP import Action, MatterMost, State

# Initialize an instance of the MatterMost class
mattermost_instance = MatterMost()

# -------------------------------------------------------------------------------
# Test: `states` method
# -------------------------------------------------------------------------------


# Test that the `states` method returns the correct list of states for given time step.
@given(st.integers(min_value=0, max_value=8))
def test_actions_return(t: int):
    assert (mattermost_instance.states(t)) == [State.DHU, State.DHC, State.DLU, State.DLC, 
                                               State.SHU, State.SHC, State.SLU, State.SHC]


# -------------------------------------------------------------------------------
# Test: `actions` method
# -------------------------------------------------------------------------------


# Test that the `actions` method returns the correct list of actions for given states.
# Note: The time step `t` is currently irrelevant for the `actions` function.
@given(st.integers(min_value=0, max_value=8))
def test_actions_return(t: int):
    assert (mattermost_instance.actions(t, State.DHU)) == [Action.Start, Action.Delay]
    assert (mattermost_instance.actions(t, State.DHC)) == [Action.Start, Action.Delay]
    assert (mattermost_instance.actions(t, State.SHU)) == [None]
    assert (mattermost_instance.actions(t, State.SHC)) == [None]


# -------------------------------------------------------------------------------
# Test: `nextFunc` method
# -------------------------------------------------------------------------------


# Test that the probabilities in the dictionary returned by `nextFunc` sum to 1.
@given(
    st.integers(min_value=0, max_value=8),        # Random time step `t`
)
def test_nextFunc_sum_equals_1(t):
    states = mattermost_instance.states(t)
    for x in states:
        actions = mattermost_instance.actions(t, x)
        for y in actions:
            res = mattermost_instance.nextFunc(t, x, y)
            assert abs(sum(res.values()) - 1.0) <= 1e-6


# Test specific cases for `nextFunc` where expected results are pre-calculated.
def test_nextFunc_actuals():
    helper_nextFunc_actuals_clean(
        {
            State.DHU: 0.049,
            State.DHC: 0.021,
            State.DLU: 0.021,
            State.DLC: 0.009,
            State.SHU: 0.243,
            State.SHC: 0.027,
            State.SLU: 0.567,
            State.SLC: 0.063,
        },
        0,
        State.DHU,
        Action.Start,
    )

    helper_nextFunc_actuals_clean(
        {State.SHU: 0.63, State.SHC: 0.07, State.SLU: 0.27, State.SLC: 0.03},
        0,
        State.SHU,
        None,
    )

    helper_nextFunc_actuals_clean({State.SHC: 0.7, State.SLC: 0.3}, 0, State.SHC, None)

    helper_nextFunc_actuals_clean({State.SHC: 0.7, State.SLC: 0.3}, 1, State.SHC, None)


# Helper function to validate the output of `nextFunc` against expected results.
def helper_nextFunc_actuals_clean(expected, t, x, y):
    pre = mattermost_instance.nextFunc(t, x, y)
    res = {state: round(prob, 3) for state, prob in pre.items()}
    assert res == expected
    res.clear()
    expected.clear()


# -------------------------------------------------------------------------------
# Test: `reward` method
# -------------------------------------------------------------------------------


# Test that the `reward` method returns the correct reward based on the next state.
@given(
    st.integers(min_value=0, max_value=8),        # Random time step `t`
)
def test_reward_states(t):
    states = mattermost_instance.states(t)
    for x in states:
        actions = mattermost_instance.actions(t, x)
        for y in actions:
            next_xs = mattermost_instance.nextFunc(t, x, y)
            for x_prim in next_xs:
                expected_reward = 1.0 if x_prim in [State.DHU, State.SHU] else 0.0
                assert mattermost_instance.reward(t, x, y, x_prim) == expected_reward


# Test that the `safe_reward` method raises appropriate errors for invalid inputs.
def test_reward_error_raised():
    # Invalid time step `t`
    with pytest.raises(ValueError):
        mattermost_instance.safe_reward(1.5, State.DHU, Action.Delay, State.DHU)
    with pytest.raises(ValueError):
        mattermost_instance.safe_reward(-1, State.DHU, Action.Delay, State.DHU)
    # Invalid state `x`
    with pytest.raises(ValueError):
        mattermost_instance.safe_reward(1, "InvalidState", Action.Start, State.DHU)
    # Invalid action `y`
    with pytest.raises(ValueError):
        mattermost_instance.safe_reward(1, State.DHU, "InvalidAction", State.DHU)
    # Invalid next state `next_x`
    with pytest.raises(ValueError):
        mattermost_instance.safe_reward(0, State.DHU, Action.Start, "invalidState")


# -------------------------------------------------------------------------------
# Test: `mMeas` method
# -------------------------------------------------------------------------------

# See: python/benchmarks/mMeas

# -------------------------------------------------------------------------------
# Test: `best` method
# -------------------------------------------------------------------------------

# See: python/benchmarks/best