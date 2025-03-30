import os
import re
import sys
import unittest

# Add the src directory to the Python path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'src')))

# Now import the function from mainFile
from mainFile import meas


class TestMeasFunction(unittest.TestCase):

    # Test the basic arithmetic och the function (val * pr)
    def test_valid_inputs(self):
        self.assertEqual(meas(2.0, 0.5), 1.0) 
        self.assertEqual(meas(-4.0, 0.25), -1.0) 
        self.assertEqual(meas(0.0, 10.0), 0.0)  


    # For invalid inputs. Basically we check if the function returns a typeError for invalid input.
    def test_invalid_inputs(self):
        with self.assertRaises(TypeError):
            meas(2, 0.5)  
        with self.assertRaises(TypeError):
            meas(2.0, "0.5")  
        with self.assertRaises(TypeError):
            meas("2.0", 0.5)  
        with self.assertRaises(TypeError):
            meas(None, 0.5)  
        with self.assertRaises(TypeError):
            meas(True, 0.7)

if __name__ == "__main__":
    unittest.main()