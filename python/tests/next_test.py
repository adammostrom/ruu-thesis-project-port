import os
import re
import sys
import unittest

# Add the src directory to the Python path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'src')))

# Now import the function from mainFile
from mainFile import nextFunc as nextFunc
from numberLineSDP import nextFunc as nextFuncSimple

class RewardTest(unittest.TestCase):
    
    def assertDictAlmostEqual(self, predicted, expected):
        self.assertEqual(set(predicted.keys()), set(expected.keys()))
        for key in expected:
            self.assertAlmostEqual(predicted[key], expected[key])

    # Checking that sum of probabilities is always 1.
    def test_sum(self):
        self.assertAlmostEqual(sum(nextFuncSimple(0, "0", "Left").values()), 1)
        self.assertAlmostEqual(sum(nextFuncSimple(0, "0", "Stay").values()), 1)
        self.assertAlmostEqual(sum(nextFuncSimple(0, "2", "Left").values()), 1)
        self.assertAlmostEqual(sum(nextFuncSimple(0, "2", "Stay").values()), 1)

        self.assertAlmostEqual(sum(nextFunc(0, "DHU", "Start").values()), 1)
        self.assertAlmostEqual(sum(nextFunc(0, "DHU", "Delay").values()), 1)
        self.assertAlmostEqual(sum(nextFunc(0, "SHU", None).values()), 1)
        self.assertAlmostEqual(sum(nextFunc(0, "SHU", None).values()), 1)

        self.assertAlmostEqual(sum(nextFunc(1, "DHU", "Start").values()), 1)
        self.assertAlmostEqual(sum(nextFunc(1, "DHU", "Delay").values()), 1)
        self.assertAlmostEqual(sum(nextFunc(1, "SHC", None).values()), 1)
        self.assertAlmostEqual(sum(nextFunc(1, "SHC", None).values()), 1)

    # Checking cases from a trivial SDP where expected results were calculated by hand.
    def test_trivial(self):
        self.assertDictAlmostEqual(nextFuncSimple(0, "0", "Left"), {'-1': 0.85, '0': 0.1, '1': 0.05})
        self.assertDictAlmostEqual(nextFuncSimple(0, "0", "Stay"), {'-1': 0.1, '0': 0.8, '1': 0.1})
        self.assertDictAlmostEqual(nextFuncSimple(0, "0", "Right"), {'-1': 0.05, '0': 0.1, '1': 0.85})

        self.assertDictAlmostEqual(nextFuncSimple(1, "2", "Left"), {'1': 0.85, '2': 0.15})
        self.assertDictAlmostEqual(nextFuncSimple(1, "2", "Stay"), {'1': 0.1, '2': 0.9})
        self.assertDictAlmostEqual(nextFuncSimple(1, "2", "Right"), {'1': 0.05, '2': 0.95})

    # Checking cases from the climate SDP where expected results were calculated by hand.
    def test_actual(self):
        self.assertDictAlmostEqual(nextFunc(0, "DHU", "Start"), {"DHU": 0.049, "DHC": 0.021, "DLU": 0.021, "DLC": 0.009, "SHU": 0.243, "SHC": 0.027, "SLU": 0.567, "SLC": 0.063})
        self.assertDictAlmostEqual(nextFunc(0, "SHU", None), {"SHU": 0.63, "SHC": 0.07, "SLU": 0.27, "SLC": 0.03})
        self.assertDictAlmostEqual(nextFunc(0, "SHC", None), {"SHC": 0.7, "SLC": 0.3})
        self.assertDictAlmostEqual(nextFunc(1, "SHC", None), {"SHC": 0.7, "SLC": 0.3})

    # def test_errors(self):
        self.assertRaises(ValueError, nextFunc, 1.5, "DHU", "Start")
        self.assertRaises(ValueError, nextFunc, -1, "DHU", "Start")
        self.assertRaises(ValueError, nextFunc, 0, "InvalidState", "Start")
        self.assertRaises(ValueError, nextFunc, 0, "DHU", "InvalidAction")
        self.assertRaises(ValueError, nextFunc, 0, "DHU", None)
        self.assertRaises(ValueError, nextFunc, 0, "SHU", "Start")

        
if __name__ == '__main__':
    unittest.main()