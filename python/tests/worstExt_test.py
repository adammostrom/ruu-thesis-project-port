import os
import sys
import unittest
from typing import Callable

# Add the src directory to the Python path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'src')))

from mainFile import worstExt, val, bi, states, actions
from numberLineSDP import worstExt as worstExtSimple

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
    unittest.main()