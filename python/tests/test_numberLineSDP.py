import math
from typing import Callable

import pytest
from hypothesis import given
from hypothesis import strategies as st
from src.implementations.numberLineSDP import Action, NumberLineSDP, State

# NumberLine instance
nli  = NumberLineSDP()



def assertDictAlmostEqual(self, predicted, expected):
    self.assertEqual(set(predicted.keys()), set(expected.keys()))
    for key in expected:
        self.assertAlmostEqual(predicted[key], expected[key])

# Checking that sum of probabilities is always 1.
def test_sum():
    assert sum(nli.nextFunc(0, State.ZERO, Action.Left).values()) == 1
    assert sum(nli.nextFunc(0, State.ZERO, Action.Stay).values()) == 1
    assert sum(nli.nextFunc(0, State.POS2, Action.Left).values()) == 1
    assert sum(nli.nextFunc(0, State.POS2, Action.Stay).values()) == 1
""" 
def test_trivial(self):
        self.assertDictAlmostEqual(nextFuncSimple(0, "0", "Left"), {'-1': 0.85, '0': 0.1, '1': 0.05})
        self.assertDictAlmostEqual(nextFuncSimple(0, "0", "Stay"), {'-1': 0.1, '0': 0.8, '1': 0.1})
        self.assertDictAlmostEqual(nextFuncSimple(0, "0", "Right"), {'-1': 0.05, '0': 0.1, '1': 0.85})

        self.assertDictAlmostEqual(nextFuncSimple(1, "2", "Left"), {'1': 0.85, '2': 0.15})
        self.assertDictAlmostEqual(nextFuncSimple(1, "2", "Stay"), {'1': 0.1, '2': 0.9})
        self.assertDictAlmostEqual(nextFuncSimple(1, "2", "Right"), {'1': 0.05, '2': 0.95})










class ValTest(unittest.TestCase):

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