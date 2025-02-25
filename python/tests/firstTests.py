import os
import re
import sys
import unittest

# Add the src directory to the Python path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'src')))

# Now import the function from mainFile
from mainFile import best


def fetchValue(x: int, y:int , state:str)->float:
    result = best (x, y, state)
    
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

    def test_0_1(self):
        self.assertEqual(fetchValue(0, 1, "DHU"), 0.46799999999999997)


if __name__ == '__main__':
    unittest.main()
