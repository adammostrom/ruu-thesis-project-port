import os
import re
import sys
import unittest

# Add the src directory to the Python path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'src')))

# Now import the function from mainFile
from mainFile import add


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
        
if __name__ == '__main__':
    unittest.main()