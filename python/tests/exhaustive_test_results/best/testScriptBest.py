import ast  # Safer than eval()
import csv
import datetime
import os
import sys
import unittest

# RUN IT WITH "python3 testScript.py"

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "../../../..", "python")))

# Import after fixing the path
from src.implementations.MatterMostSDP import MatterMost, State

sdp_instance = MatterMost()

class TestBestsFunctionFromCSV(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        """Load test cases from CSV file once before running tests."""
        cls.test_cases = []
        with open("best_func_test_cases.csv", mode="r") as file:
            reader = csv.DictReader(file)
            for row in reader:
                cls.test_cases.append(row)

    def run_bests(self, state, x, y):
        """Mock function to simulate bests output (replace with actual function call)."""
        result = sdp_instance.best(x, y, state)  # `best` returns a string
        return result

    def test_bests_from_csv(self):
        """Run all test cases from the CSV file dynamically and log results to a file."""
        with open("best_func_test_results.txt", "w") as log_file:
            
                        # Info box
            log_file.write("++------------------------------------------------------------------------------------+\n")
            log_file.write("|                                        Test Run Info                                |\n")
            log_file.write("+-------------------------------------------------------------------------------------+\n")
            log_file.write(f"| Date       : {datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
            log_file.write(f"| Test       : best function CSV test\n")
            log_file.write(f"| Run by     : Adam\n")
            log_file.write(f"| Description: Automated tests for best using CSV inputs.\n")
            log_file.write("+------------------------------------------------------------------------------------+\n\n")
            
            
            
            for test in self.test_cases:
                with self.subTest(state=test["State"], xy=test["Inputs"]):
                    x, y = ast.literal_eval(test["Inputs"])
                    state = State[test["State"]]

                    result = self.run_bests(state, x, y)

                    parts = result.split(":")[1].strip().split(", ")
                    horizon = int(parts[0])
                    best_action = parts[1].split('.')[-1]  # Strip enum prefix
                    best_action_name = best_action.split('.')[-1]  # 'Delay'
                    value = float(parts[2])

                    expected_horizon = int(test["Horizon"])
                    expected_best = test["Best"]
                    expected_value = float(test["Value"])

                    try:
                        self.assertEqual(horizon, expected_horizon)
                        self.assertEqual(best_action_name, expected_best)
                        self.assertAlmostEqual(value, expected_value, places=5)
                        log_file.write(f"[PASS] state={state}, time={x}, horizon={y}\n")
                    except AssertionError as e:
                        log_file.write(f"[FAIL] state={state}, time={x}, horizon={y}: {str(e).split(':')[-1].strip()}\n")

                    log_file.write(f"Expected: Horizon={expected_horizon}, Best={expected_best}, Value={expected_value}\n")
                    log_file.write(f"Actual  : Horizon={horizon}, Best={best_action_name}, Value={value}\n")
                    log_file.write("+------------------------------------------------------------------------------------+\n")

if __name__ == "__main__":
    unittest.main()
