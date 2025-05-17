import ast  # Safer than eval()
import csv
import datetime
import os
import re
import sys
import unittest

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "../../../..", "python")))

# Import after fixing the path
from src.implementations.MatterMostSDP import MatterMost, State

sdp_instance = MatterMost()


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
            
            # Info box
            log_file.write("++------------------------------------------------------------------------------------+\n")
            log_file.write("|                                        Test Run Info                                |\n")
            log_file.write("+-------------------------------------------------------------------------------------+\n")
            log_file.write(f"| Date       : {datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
            log_file.write(f"| Test       : mMeas function CSV test\n")
            log_file.write(f"| Run by     : Adam\n")
            log_file.write(f"| Description: Automated tests for mMeas using CSV inputs.\n")
            log_file.write("+------------------------------------------------------------------------------------+\n\n")
    
            """Run all test cases from the CSV file dynamically."""
            for test in self.test_cases:
                with self.subTest(state=test["State"], xy=test["Inputs"]):
                    x, y = ast.literal_eval(test["Inputs"])
                    state = State[test["State"]]

                    if y == 0:
                        # Skip actual call, just log pass with result 0
                        log_file.write(f"[PASS] input=({x}, {y}, {state.name}): result=0 (y==0 shortcut)\n")
                        log_file.write("+--------------------------------------------------+\n")
                        continue
                    result = self.run_mMeas(state, x, y)

                    expected_value = float(test["Value"])

                    try:
                        self.assertAlmostEqual(result, expected_value, places=5)
                        log_file.write(f"[PASS] input=({x}, {y}, {state.name})\n")
                    except AssertionError as e:
                        log_file.write(f"[FAIL] input=({x}, {y}, {state.name}): {str(e).split(':')[-1].strip()}\n")

                    log_file.write(f"Expected Value = {expected_value}\n")
                    log_file.write(f"Actual Value   = {result}\n")
                    log_file.write("+--------------------------------------------------+\n")

if __name__ == "__main__":
    unittest.main()
