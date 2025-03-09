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
    
    def test_trivial(self):
        self.assertEqual(nextFuncSimple(0, "-2", "Left"), "{'-2': 0.1, '-1': 0.8, '0': 0.1}")

    def test_edge(self):
        self.assertEqual(nextFunc(0, "DHU", "Start"), 1)
        self.assertEqual(nextFunc(0, "DHU", "Start"), 0)
        self.assertEqual(nextFunc(0, "DHU", "Start"), 0)
        self.assertEqual(nextFunc(0, "DHU", "Start"), 0)
        self.assertEqual(nextFunc(0, "DHU", "Start"), 1)
        self.assertEqual(nextFunc(0, "DHU", "Start"), 0)
        self.assertEqual(nextFunc(0, "DHU", "Start"), 0)

    def test_errors(self):
        self.assertRaises(ValueError, nextFunc, 1.5, "DHU", "Start")
        self.assertRaises(ValueError, nextFunc, -1, "DHU", "Start")

        self.assertRaises(ValueError, nextFunc, 0, "InvalidState", "Start")
        
        self.assertRaises(ValueError, nextFunc, 0, "DHU", "InvalidAction")

        
if __name__ == '__main__':
    unittest.main()