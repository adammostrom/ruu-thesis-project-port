import sys, os
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

import math

from typing import Callable

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
def test_actions_return(t: int):
    assert len(nli.states(t)) == 2*t+1


# def assertDictAlmostEqual(self, predicted, expected):
#     self.assertEqual(set(predicted.keys()), set(expected.keys()))
#     for key in expected:
#         self.assertAlmostEqual(predicted[key], expected[key])

""" 
def test_trivial(self):
        self.assertDictAlmostEqual(nextFuncSimple(0, "0", "Left"), {'-1': 0.85, '0': 0.1, '1': 0.05})
        self.assertDictAlmostEqual(nextFuncSimple(0, "0", "Stay"), {'-1': 0.1, '0': 0.8, '1': 0.1})
        self.assertDictAlmostEqual(nextFuncSimple(0, "0", "Right"), {'-1': 0.05, '0': 0.1, '1': 0.85})

        self.assertDictAlmostEqual(nextFuncSimple(1, "2", "Left"), {'1': 0.85, '2': 0.15})
        self.assertDictAlmostEqual(nextFuncSimple(1, "2", "Stay"), {'1': 0.1, '2': 0.9})
        self.assertDictAlmostEqual(nextFuncSimple(1, "2", "Right"), {'1': 0.05, '2': 0.95})


    # Checking cases from a trivial SDP where expected results were calculated by hand.
    def test_trivial(self):
        ps1 = [
                {'-2': 'Stay', '-1': 'Stay', '0': 'Stay', '1': 'Right', '2': 'Right'}
                        ]
        ps2 = [
                {'-2': 'Stay', '-1': 'Stay', '0': 'Right', '1': 'Right', '2': 'Right'},
                {'-2': 'Stay', '-1': 'Stay', '0': 'Stay', '1': 'Right', '2': 'Right'}
                        ]

        self.assertAlmostEqual(valSimple(0, ps1, "0"), 0.0)
        self.assertAlmostEqual(valSimple(0, ps1, "2"), 0.95)
        self.assertAlmostEqual(valSimple(1, ps1, "0"), 0.0)
        self.assertAlmostEqual(valSimple(1, ps1, "2"), 0.95)

        self.assertAlmostEqual(valSimple(0, ps2, "0"), 0.7225)
        self.assertAlmostEqual(valSimple(0, ps2, "2"), 1.895)
        self.assertAlmostEqual(valSimple(1, ps2, "0"), 0.7225)
        self.assertAlmostEqual(valSimple(1, ps2, "2"), 1.895)
        
  
  
  
"""  


# -------------------------------------------------------------------------------
# Test: `actions` method
# -------------------------------------------------------------------------------

# Test that the `actions` method returns the correct list of actions for given states.
# Note: The time step `t` is currently irrelevant for the `actions` function.
@given(st.integers(min_value=0, max_value=8))
def test_actions_return(t: int):
    assert (nli.actions(t, State.DHU)) == [Action.Start, Action.Delay]
    assert (nli.actions(t, State.DHC)) == [Action.Start, Action.Delay]
    assert (nli.actions(t, State.SHU)) == [None]
    assert (nli.actions(t, State.SHC)) == [None]

# Test that the `safe_actions` method raises appropriate errors for invalid inputs.
def test_actions_error_raised():
    # Invalid time step `t`
    with pytest.raises(ValueError):
        nli.safe_actions(1.5, State.DHU)
    with pytest.raises(ValueError):
        nli.safe_actions(-1, State.DHU)
    # Invalid state `x`
    with pytest.raises(ValueError):
        nli.safe_actions(1, "InvalidState")

# -------------------------------------------------------------------------------
# Test: `nextFunc` method
# -------------------------------------------------------------------------------

# Test that the probabilities in the dictionary returned by `nextFunc` sum to 1.
@given(
    st.integers(min_value=0, max_value=8),        # Random time step `t`
)
def test_nextFunc_sum_equals_1(t):
    states = nli.states(t)
    for x in states:
        actions = nli.actions(t, x)
        for y in actions:
            res = nli.nextFunc(t, x, y)
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
    pre = nli.nextFunc(t, x, y)
    res = {state: round(prob, 3) for state, prob in pre.items()}
    assert res == expected
    res.clear()
    expected.clear()

# Test that the `safe_nextFunc` method raises appropriate errors for invalid inputs.
def test_nextFunc_error_raised():
    # Invalid time step `t`
    with pytest.raises(ValueError):
        nli.safe_nextFunc(1.5, State.DHU, Action.Delay)
    with pytest.raises(ValueError):
        nli.safe_nextFunc(-1, State.DHU, Action.Delay)
    # Invalid state `x`
    with pytest.raises(ValueError):
        nli.safe_nextFunc(1, "InvalidState", Action.Start)
    # Invalid action `y`
    with pytest.raises(ValueError):
        nli.safe_nextFunc(1, State.DHU, "InvalidAction")

# -------------------------------------------------------------------------------
# Test: `reward` method
# -------------------------------------------------------------------------------

# Test that the `reward` method returns the correct reward based on the next state.
@given(
    st.integers(min_value=0, max_value=8),        # Random time step `t`
)
def test_reward_states(t):
    states = nli.states(t)
    for x in states:
        actions = nli.actions(t, x)
        for y in actions:
            next_xs = nli.nextFunc(t, x, y)
            for x_prim in next_xs:
                expected_reward = 1.0 if x_prim in [State.DHU, State.SHU] else 0.0
                assert nli.reward(t, x, y, x_prim) == expected_reward

# Test that the `safe_reward` method raises appropriate errors for invalid inputs.
def test_reward_error_raised():
    # Invalid time step `t`
    with pytest.raises(ValueError):
        nli.safe_reward(1.5, State.DHU, Action.Delay, State.DHU)
    with pytest.raises(ValueError):
        nli.safe_reward(-1, State.DHU, Action.Delay, State.DHU)
    # Invalid state `x`
    with pytest.raises(ValueError):
        nli.safe_reward(1, "InvalidState", Action.Start, State.DHU)
    # Invalid action `y`
    with pytest.raises(ValueError):
        nli.safe_reward(1, State.DHU, "InvalidAction", State.DHU)
    # Invalid next state `next_x`
    with pytest.raises(ValueError):
        nli.safe_reward(0, State.DHU, Action.Start, "invalidState")

# -------------------------------------------------------------------------------
# Test: `mMeas` method
# -------------------------------------------------------------------------------

# See: python/benchmarks/mMeas

# -------------------------------------------------------------------------------
# Test: `best` method
# -------------------------------------------------------------------------------

# See: python/benchmarks/best
  
  
  
  
  
  
  
  
  
        

def isOptimalExtension(
        best_p: dict[str, str],
        ps_tail: list[dict[str, str]],
        t: int,
        states: list[str],
        actions: Callable[[str], list[str] | list[None]]
        ) -> bool:

    for x in states:
        best_val = val(t, [best_p] + ps_tail, x)
        for y in actions(x):
            other_p = {x : y}
            other_val = val(t, [other_p] + ps_tail, x)
            if other_val > best_val:
                return False
    return True

class BestExtTest(unittest.TestCase):
    # Checking optimality using isOptimalExtension
    def test_property(self):
        for i in range(7):
            ps_tail = bi(1, i)
            self.assertEqual(isOptimalExtension(bestExt(0, ps_tail), ps_tail, 0, states, actions), True)
        for t in range(7):
            ps_tail = bi(t+1, 5)
            self.assertEqual(isOptimalExtension(bestExt(0, ps_tail), ps_tail, t, states, actions), True)

    # Checking cases from a trivial SDP where expected results were calculated by hand.
    def test_trivial(self):
        ps1 = [
                {'-2': 'Stay', '-1': 'Stay', '0': 'Stay', '1': 'Right', '2': 'Right'}
                        ]
        ps2 = [
                {'-2': 'Stay', '-1': 'Stay', '0': 'Right', '1': 'Right', '2': 'Right'},
                {'-2': 'Stay', '-1': 'Stay', '0': 'Stay', '1': 'Right', '2': 'Right'}
                        ]

        self.assertEqual(bestExtSimple(0, ps1), {'-2': 'Stay', '-1': 'Stay', '0': 'Right', '1': 'Right', '2': 'Right'})
        self.assertEqual(bestExtSimple(0, ps2), {'-2': 'Stay', '-1': 'Right', '0': 'Right', '1': 'Right', '2': 'Right'})
        self.assertEqual(bestExtSimple(1, ps1), {'-2': 'Stay', '-1': 'Stay', '0': 'Right', '1': 'Right', '2': 'Right'})
        self.assertEqual(bestExtSimple(1, ps2), {'-2': 'Stay', '-1': 'Right', '0': 'Right', '1': 'Right', '2': 'Right'})

    # Checking that function returns errors for invalid inputs.
    def test_errors(self):
        ps = []
        self.assertRaises(ValueError, bestExt, -1, ps)
        self.assertRaises(ValueError, bestExt, 1.5, ps)
        self.assertRaises(TypeError, bestExt, 0, "Invalid_policy_list")
        
        

















        
def isOptimalExtension(
        best_p: dict[str, str],
        ps_tail: list[dict[str, str]],
        t: int,
        states: list[str],
        actions: Callable[[str], list[str] | list[None]]
        ) -> bool:

    for x in states:
        best_val = val(t, [best_p] + ps_tail, x)
        for y in actions(x):
            other_p = {x : y}
            other_val = val(t, [other_p] + ps_tail, x)
            if other_val > best_val:
                return False
    return True

class biTest(unittest.TestCase):
    # Checking that all policies created by the function are optimal extensions.
    def test_property(self):
        ps = bi(1, 7)
        for i in range(7):
            self.assertEqual(isOptimalExtension(ps[i], ps[i+1:], i, states, actions), True)
            self.assertEqual(isOptimalExtension(ps[i], ps[i+1:], i+1, states, actions), True)

    # Checking cases from a trivial SDP where expected results were calculated by hand.
    def test_trivial(self):
        self.assertEqual(biSimple(0, 1), [{'-2': 'Stay', '-1': 'Stay', '0': 'Stay', '1': 'Right', '2': 'Right'}])
        self.assertEqual(biSimple(0, 2), [{'-2': 'Stay', '-1': 'Stay', '0': 'Right', '1': 'Right', '2': 'Right'},
                                    {'-2': 'Stay', '-1': 'Stay', '0': 'Stay', '1': 'Right', '2': 'Right'}])
        self.assertEqual(biSimple(1, 1), [{'-2': 'Stay', '-1': 'Stay', '0': 'Stay', '1': 'Right', '2': 'Right'}])
        self.assertEqual(biSimple(1, 2), [{'-2': 'Stay', '-1': 'Stay', '0': 'Right', '1': 'Right', '2': 'Right'},
                                    {'-2': 'Stay', '-1': 'Stay', '0': 'Stay', '1': 'Right', '2': 'Right'}])

    # Checking that function returns errors for invalid inputs.
    def test_errors(self):
        self.assertRaises(ValueError, bi, -1, 1)
        self.assertRaises(ValueError, bi, 1.5, 1)
        self.assertRaises(ValueError, bi, 0, -1)
        self.assertRaises(ValueError, bi, 0, 1.5)
        
        
            # Checking cases from a trivial SDP where expected results were calculated by hand.
    def test_trivial(self):
        self.assertDictAlmostEqual(nextFuncSimple(0, "0", "Left"), {'-1': 0.85, '0': 0.1, '1': 0.05})
        self.assertDictAlmostEqual(nextFuncSimple(0, "0", "Stay"), {'-1': 0.1, '0': 0.8, '1': 0.1})
        self.assertDictAlmostEqual(nextFuncSimple(0, "0", "Right"), {'-1': 0.05, '0': 0.1, '1': 0.85})

        self.assertDictAlmostEqual(nextFuncSimple(1, "2", "Left"), {'1': 0.85, '2': 0.15})
        self.assertDictAlmostEqual(nextFuncSimple(1, "2", "Stay"), {'1': 0.1, '2': 0.9})
        self.assertDictAlmostEqual(nextFuncSimple(1, "2", "Right"), {'1': 0.05, '2': 0.95})

        
        
        
        
        
        
        
        
        
        
class AddTest(unittest.TestCase):

    def test_trivial(self):
        self.assertEqual(add(0.0, 0.0), 0.0)
        self.assertEqual(add(-1.0, -1.0), -2.0)
        self.assertEqual(add(-1.0, 1.0), 0.0)
        self.assertAlmostEqual(add(1.1, 2.2), 3.3)

    def test_errors(self):
        self.assertRaises(TypeError, add, 1, 1)
        self.assertRaises(TypeError, add, 1.0, 1)
        self.assertRaises(TypeError, add, "notFloat", 1.0)
        
        
        
        
        
        
class TestMeasFunction(unittest.TestCase):

    # Test the basic arithmetic och the function (val * pr)
    def test_valid_inputs(self):
        self.assertEqual(meas(2.0, 0.5), 1.0) 
        self.assertEqual(meas(-4.0, 0.25), -1.0) 
        self.assertEqual(meas(0.0, 10.0), 0.0)  


    # For invalid inputs. Basically we check if the function returns a typeError for invalid input.
    def test_invalid_inputs(self):
        with self.assertRaises(TypeError):
            meas(2, 0.5)  
        with self.assertRaises(TypeError):
            meas(2.0, "0.5")  
        with self.assertRaises(TypeError):
            meas("2.0", 0.5)  
        with self.assertRaises(TypeError):
            meas(None, 0.5)  
        with self.assertRaises(TypeError):
            meas(True, 0.7)
            
            
            
            
            
            
            
            
            
def isWorstExtension(
        best_p: dict[str, str],
        ps_tail: list[dict[str, str]],
        t: int,
        states: list[str],
        actions: Callable[[str], list[str] | list[None]]
        ) -> bool:

    for x in states:
        best_val = val(t, [best_p] + ps_tail, x)
        for y in actions(x):
            other_p = {x : y}
            other_val = val(t, [other_p] + ps_tail, x)
            if other_val < best_val:
                return False
    return True

class WorstExtTest(unittest.TestCase):
    # Checking property fulfillment using isWorstExtension.
    def test_property(self):
        for i in range(7):
            ps_tail = bi(1, i)
            self.assertEqual(isWorstExtension(worstExt(0, ps_tail), ps_tail, 0, states, actions), True)
        for t in range(7):
            ps_tail = bi(t+1, 5)
            self.assertEqual(isWorstExtension(worstExt(0, ps_tail), ps_tail, t, states, actions), True)

    # Checking cases from a trivial SDP where expected results were calculated by hand.
    def test_trivial(self):
        ps1 = [
                {'-2': 'Stay', '-1': 'Stay', '0': 'Stay', '1': 'Right', '2': 'Right'}
                        ]
        ps2 = [
                {'-2': 'Stay', '-1': 'Stay', '0': 'Right', '1': 'Right', '2': 'Right'},
                {'-2': 'Stay', '-1': 'Stay', '0': 'Stay', '1': 'Right', '2': 'Right'}
                        ]

        self.assertEqual(worstExtSimple(0, ps1), {'-2': 'Stay', '-1': 'Stay', '0': 'Left', '1': 'Left', '2': 'Left'})
        self.assertEqual(worstExtSimple(0, ps2), {'-2': 'Stay', '-1': 'Left', '0': 'Left', '1': 'Left', '2': 'Left'})
        self.assertEqual(worstExtSimple(1, ps1), {'-2': 'Stay', '-1': 'Stay', '0': 'Left', '1': 'Left', '2': 'Left'})
        self.assertEqual(worstExtSimple(1, ps2), {'-2': 'Stay', '-1': 'Left', '0': 'Left', '1': 'Left', '2': 'Left'})

    # Checking that function returns errors for invalid inputs.
    def test_errors(self):
        ps = []
        self.assertRaises(ValueError, worstExt, -1, ps)
        self.assertRaises(ValueError, worstExt, 1.5, ps)
        self.assertRaises(TypeError, worstExt, 0, "Invalid_policy_list")
        
        
        
        
        
        
        
        
if __name__ == '__main__':
    unittest.main() """