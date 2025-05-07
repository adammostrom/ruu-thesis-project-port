import ast  # Safer than eval()
import csv
import os
import re
import sys
import unittest

# Import after fixing the path
from src.implementations.MatterMostMemo import MatterMost as module

sdp_instance = module()


class TestmMeasFunctionFromCSV(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        """Load test cases from CSV file once before running tests."""
        cls.test_cases = []
        with open("mMeas_test_cases.csv", mode="r") as file:
            reader = csv.DictReader(file)
            for row in reader:
                cls.test_cases.append(row)

    def run_mMeas(self, state, x, y):
        result = sdp_instance.mMeas(x, y, state)  # `mMeas` returns a float
        return result

    def test_mMeas_from_csv(self):
        
        """Run all test cases from the CSV file dynamically and log results to a file."""
        with open("mMeas_test_results.txt", "w") as log_file:  # Open log file
    
            """Run all test cases from the CSV file dynamically."""
            for test in self.test_cases:
                with self.subTest(state=test["State"], xy=test["Inputs"]):
                    
                    # Convert input "(0,7)" to (0,7) and unpack into x and y
                    x, y = ast.literal_eval(test["Inputs"])  
                    state = test["State"]

                    print(f"Testing with: state={state}, x={x}, y={y}")

                    result = self.run_mMeas(state, x, y)
                    
                    print(f"Raw output from mMeas: {result}")

                    # Expected values from CSV
                    expected_value = float(test["Value"])
                    

                    try:
                        self.assertAlmostEqual(result, expected_value, places=5)
                        log_file.write("[PASS]")
                    except AssertionError as e:
                        # Write only the short error message
                        log_file.write(f"[FAIL]: {str(e).split(':')[-1].strip()}\n")
                        
                    log_file.write(f" for input = {x} {y} {state}\n")
                    log_file.write(f"Expected Value        = {expected_value}\n")
                    log_file.write(f"Actual Value          = {result}\n")

                    log_file.write(f"+--------------------------------------------------+")

                    log_file.write("\n")  # Add spacing between test cases

if __name__ == "__main__":
    unittest.main()
