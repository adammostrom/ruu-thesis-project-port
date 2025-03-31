import os
import re
import sys
import unittest

# Add the src directory to the Python path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'src')))

# Now import the function from mainFile
from mainFile import reward

class RewardTest(unittest.TestCase):

    def test_states(self):
        self.assertEqual(reward(0, "DHU", "Start", "DHU"), 1)
        self.assertEqual(reward(0, "DHU", "Start", "DHC"), 0)
        self.assertEqual(reward(0, "DHU", "Start", "DLU"), 0)
        self.assertEqual(reward(0, "DHU", "Start", "DLC"), 0)
        self.assertEqual(reward(0, "DHU", "Start", "SHU"), 1)
        self.assertEqual(reward(0, "DHU", "Start", "SHC"), 0)
        self.assertEqual(reward(0, "DHU", "Start", "SLU"), 0)
        self.assertEqual(reward(0, "DHU", "Start", "SLC"), 0)

        self.assertEqual(reward(0, "DHU", "Delay", "DHU"), 1)
        self.assertEqual(reward(0, "DHU", "Delay", "DHC"), 0)
        self.assertEqual(reward(0, "DHU", "Delay", "DLU"), 0)
        self.assertEqual(reward(0, "DHU", "Delay", "DLC"), 0)
        self.assertEqual(reward(0, "DHU", "Delay", "SHU"), 1)
        self.assertEqual(reward(0, "DHU", "Delay", "SHC"), 0)
        self.assertEqual(reward(0, "DHU", "Delay", "SLU"), 0)
        self.assertEqual(reward(0, "DHU", "Delay", "SLC"), 0)
       
        self.assertEqual(reward(0, "SHU", None, "DHU"), 1)
        self.assertEqual(reward(0, "SHU", None, "DHC"), 0)
        self.assertEqual(reward(0, "SHU", None, "DLU"), 0)
        self.assertEqual(reward(0, "SHU", None, "DLC"), 0)
        self.assertEqual(reward(0, "SHU", None, "SHU"), 1)
        self.assertEqual(reward(0, "SHU", None, "SHC"), 0)
        self.assertEqual(reward(0, "SHU", None, "SLU"), 0)
        self.assertEqual(reward(0, "SHU", None, "SLC"), 0)

    def test_errors(self):
        self.assertRaises(ValueError, reward, 1.5, "DHU", "Delay", "DHU")
        self.assertRaises(ValueError, reward, -1, "DHU", "InvalidAction", "DHU")

        self.assertRaises(ValueError, reward, 0, "InvalidState", "Start", "DHU")
        self.assertRaises(ValueError, reward, 0, "DHU", "InvalidAction", "DHU")
        
        self.assertRaises(ValueError, reward, 0, "DHU", None, "DHU")
        self.assertRaises(ValueError, reward, 0, "SHU", "Start", "DHU")
        
        self.assertRaises(ValueError, reward, 0, "DHU", "Start", "InvalidState")
        
if __name__ == '__main__':
    unittest.main()

