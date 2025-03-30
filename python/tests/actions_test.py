import os
import re
import sys
import unittest

# Add the src directory to the Python path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'src')))

# Now import the function from mainFile
from mainFile import actions


class ActionsTest(unittest.TestCase):

    # Checking some cases that can easily be verified by hand.
    def test_trivial(self):
        self.assertEqual(actions("DHU"), ["Start", "Delay"])
        self.assertEqual(actions("DHC"), ["Start", "Delay"])
        self.assertEqual(actions("SLU"), [None])
        self.assertEqual(actions("SLC"), [None])

    # Maybe add tests from original SDP where answer has been calculated by hand?

    def test_errors(self):
        self.assertRaises(ValueError, actions, "invalidState")
        self.assertRaises(ValueError, actions, None)
        
if __name__ == '__main__':
    unittest.main()