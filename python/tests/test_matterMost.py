import math

import pytest
from hypothesis import given
from hypothesis import strategies as st
from src.implementations.MatterMostSDP import Action, MatterMost, State

# Initialize an instance of the MatterMost class
mattermost_instance = MatterMost()

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
    st.sampled_from(mattermost_instance.states),  # Random state `x`
)
def test_nextFunc_sum_equals_1(t, x):
    actionsT = mattermost_instance.actions(t, x)
    for y in actionsT:
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
    st.sampled_from(mattermost_instance.states),  # Random state `x`
    st.sampled_from(mattermost_instance.states),  # Random next state `next_x`
)
def test_reward_states(t, x, next_x):
    y = mattermost_instance.actions(t, x)
    expected_reward = 1.0 if next_x in [State.DHU, State.SHU] else 0.0
    assert mattermost_instance.reward(t, x, y[0], next_x) == expected_reward

# Test that the `reward` method raises appropriate errors for invalid inputs.
def test_reward_error_raised():
    # Invalid time step `t`
    with pytest.raises(ValueError):
        mattermost_instance.reward(1.5, State.DHU, Action.Delay, State.DHU)
    with pytest.raises(ValueError):
        mattermost_instance.reward(-1, State.DHU, Action.Delay, State.DHU)
    # Invalid state `x`
    with pytest.raises(ValueError):
        mattermost_instance.reward(1, "InvalidState", Action.Start, State.DHU)
    # Invalid action `y`
    with pytest.raises(ValueError):
        mattermost_instance.reward(1, State.DHU, "InvalidAction", State.DHU)
    with pytest.raises(ValueError):
        mattermost_instance.reward(0, State.DHU, None, State.DHU)
    # Invalid next state `next_x`
    with pytest.raises(ValueError):
        mattermost_instance.reward(0, State.DHU, Action.Start, "invalidState")

# -------------------------------------------------------------------------------
# Test: `mkSimpleProb` method
# -------------------------------------------------------------------------------

def test_mkSimpleProb_valid_probabilities():
        pairs = [(State.DHU, 0.6), (State.DHC, 0.4)]
        result = mattermost_instance.mkSimpleProb(pairs)
        assert (result) == {State.DHU: 0.6, State.DHC: 0.4}
        
def test_mkSimpleProb_negative_probability():
    pairs = [(State.SHU, -0.1), (State.DHU, 1.1)]
    with pytest.raises(ValueError):
        mattermost_instance.mkSimpleProb(pairs)

def test_mkSimpleProb_probabilities_do_not_sum_to_one():
    pairs = [(State.SLC, 0.5), (State.DHC, 0.3)]
    with pytest.raises(ValueError):
        mattermost_instance.mkSimpleProb(pairs)

def test_mkSimpleProb_probabilities_sum_close_to_one():
    pairs = [(State.SLU, 0.50000001), (State.DLC, 0.49999999)]
    result = mattermost_instance.mkSimpleProb(pairs)
    assert result == {State.SLU: 0.50000001, State.DLC: 0.49999999}

# -------------------------------------------------------------------------------
# Test: `mMeas` method
# -------------------------------------------------------------------------------

# See: python/benchmarks/mMeas

# -------------------------------------------------------------------------------
# Test: `best` method
# -------------------------------------------------------------------------------

# See: python/benchmarks/best

# -------------------------------------------------------------------------------
# Test: `val` method
# -------------------------------------------------------------------------------

def test_val_with_empty_policy_list():
    result = mattermost_instance.val(0, [], State.DHU)
    assert result == 0.0

def test_val_with_invalid_time_step():
    with pytest.raises(ValueError):
        mattermost_instance.val(-1, [], State.DHU)

def test_val_with_invalid_state():
    with pytest.raises(ValueError):
        mattermost_instance.val(0, [], State("Invalid"))

def test_val_with_state_not_in_policy():
    ps = [{State.SHU: Action.Start}]
    result = mattermost_instance.val(0, ps, State.DHU)
    assert result == 0.0

# -------------------------------------------------------------------------------
# Test: `bestExt` method
# -------------------------------------------------------------------------------

# -------------------------------------------------------------------------------
# Test: `worstExt` method
# -------------------------------------------------------------------------------


# TODO: Discuss wether bestExt and worstExt should return smaller dicts, as now they both return all possible state:action pairs for that time step, hence they operate based on the "t" variable. Testing different length of input list of dicts yields same result. Also, as of yet no error correction implemented.

# - worstExt

# -------------------------------------------------------------------------------
# Test: `bi` method
# -------------------------------------------------------------------------------

def test_bi_with_n_zero():
    result = mattermost_instance.bi(0, 0)
    assert result == []

def test_bi_with_n_one():
    ps_best = mattermost_instance.bestExt(0, [])
    result = mattermost_instance.bi(0, 1)
    assert result == [ps_best]

def test_bi_with_n_two():
    ps_best_1 = mattermost_instance.bestExt(1, [])
    ps_best_0 = mattermost_instance.bestExt(0, [ps_best_1])
    result = mattermost_instance.bi(0, 2)
    assert result == [ps_best_0, ps_best_1]

""" def test_bi_with_invalid_n():
    with pytest.raises(ValueError):
        mattermost_instance.bi(0, -1) """
        # TODO: Implement error checking for negative time steps in "bi".

def test_bi_with_invalid_time_step():
    with pytest.raises(ValueError):
        mattermost_instance.bi(-1, 1)

def test_bi_with_large_n():
    result = mattermost_instance.bi(0, 5)
    assert len(result) == 5
    for i in range(5):
        assert isinstance(result[i], dict)

