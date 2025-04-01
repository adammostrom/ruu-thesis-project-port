import os
import sys
import unittest
from typing import Callable

# Add the src directory to the Python path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'src')))

from mainFile import bestExt, val, bi, states, actions
from numberLineSDP import bi as biSimple

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
        
if __name__ == '__main__':
    unittest.main()