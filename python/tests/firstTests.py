import os
import re
import sys
import unittest

# Add the src directory to the Python path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'src')))

# Now import the function from mainFile
from mainFile import nextFunc, reward, meas, val, bestExt, bi, best, mMeas 

def bestFetch(x: int, y: int , state: str) -> float:
    result = best(x, y, state)
    
    # Regular expression to find the last number
    match = re.search(r"(\d+\.\d+|\d+)(?=\s*$)", result)

    if match:
        last_number = float(match.group(0))
        print(last_number)  # This will print: 0.46799999999999997
    else:
        print("No number found")
    return last_number

"""  
Börjar med att definera en class per state, och i varje class metod per mainFile function.
"""

class TestDHU(unittest.TestCase):

    def test_next(self):
        #self.assertEqual(next(0, "DHU", "Start"), )  Unfinished
        #self.assertEqual(next(0, "DHU", "Delay"), )  Unfinished
        pass

    def test_reward(self):
        self.assertEqual(reward(0, "DHU", "Start", "DHU"), 1)
        self.assertEqual(reward(0, "DHU", "Start", "DHC"), 0)
        self.assertEqual(reward(0, "DHU", "Start", "DLU"), 0)
        self.assertEqual(reward(0, "DHU", "Start", "DLC"), 0)
        self.assertEqual(reward(0, "DHU", "Start", "SHU"), 1)
        self.assertEqual(reward(0, "DHU", "Start", "SHC"), 0)
        self.assertEqual(reward(0, "DHU", "Start", "SLU"), 0)
        self.assertEqual(reward(0, "DHU", "Start", "SLC"), 0)

    def test_meas(self):
        self.assertEqual(meas(1, 1), 1)
        # Add more test cases

    def test_val(self):
        self.assertEqual(val(0, [], "DHU"), 0) # Policy list of length 0 has the value of 0.
        # För att testa fler cases här kanske vi bör skapa en fil separat fil med några "dummy policy-listor"
        # som är enkla nog att kunna räkna ut värdet på för hand. Isf kan vi sen importera dessa och testa här.

    def test_bestExt(self):
        pass

    def test_bi(self):
        pass

    def test_best(self):
        # Valid inputs
        # self.assertEqual(bestFetch(0, 1, "DHU"), 0.46799999999999997)
        self.assertEqual(best(0, 1, "DHU"), "Horizon, best, value : 1, Delay, 0.46799999999999997")
        # self.assertEqual(best(0, 2, "DHU"), IDRIS OUTPUT)
        # self.assertEqual(best(0, 3, "DHU"), IDRIS OUTPUT)
        # self.assertEqual(best(0, 4, "DHU"), IDRIS OUTPUT)
        # self.assertEqual(best(0, 5, "DHU"), IDRIS OUTPUT)
        # self.assertEqual(best(0, 6, "DHU"), IDRIS OUTPUT)
        # self.assertEqual(best(0, 7, "DHU"), IDRIS OUTPUT)
        # self.assertEqual(best(0, 8, "DHU"), IDRIS OUTPUT)

        # Edge cases / Error handling
        # self.assertRaises(ValueError, best, -1, 1, "DHU")
        # self.assertRaises(ValueError, best, 0, -1, "DHU")
        # self.assertRaises(ValueError, best, 0, 0, "DHU")
        # self.assertRaises(ValueError, best, 0, 1.5, "DHU")

    def test_mMeas(self):
        self.assertEqual(mMeas(0, 4, "SHU"), 0)
        self.assertEqual(mMeas(0, 6, "SHC"), 0)
        self.assertEqual(mMeas(0, 7, "DHU"), 0.1730602684132721) # Results differ but only on the 16th decimal.
        self.assertEqual(mMeas(1, 7, "DHU"), 0.5673067719100584)
        self.assertEqual(mMeas(3, 7, "DHU"), 0.5673067719100584)

if __name__ == '__main__':
    unittest.main()
