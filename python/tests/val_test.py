import os
import re
import sys
import unittest

# Add the src directory to the Python path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'src')))

# Now import the function from mainFile
from mainFile import val as val
from numberLineSDP import val as valSimple

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

    # Maybe add tests from original SDP where answer has been calculated by hand?

    def test_errors(self):
        ps = [{"DHU": 1.0}]
        self.assertRaises(ValueError, val, -1, ps, "DHU")
        self.assertRaises(ValueError, val, 1.5, ps, "DHU")
        self.assertRaises(TypeError, val, 0, None, "DHU")
        self.assertRaises(ValueError, val, 0, ps, None)
        
if __name__ == '__main__':
    unittest.main()