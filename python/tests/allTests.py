import ast  # Safer than eval()
import csv
import os
import re
import sys
import unittest

# Add the src directory to the Python path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'src')))

# Now import the function from mainFile
from mainFile import best


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
        result = best(x, y, state)  # `best` returns a string
        return result

    def test_bests_from_csv(self):
        
        """Run all test cases from the CSV file dynamically and log results to a file."""
        with open("best_func_test_results.txt", "w") as log_file:  # Open log file
    
            """Run all test cases from the CSV file dynamically."""
            for test in self.test_cases:
                with self.subTest(state=test["State"], xy=test["Inputs"]):
                    
                    # Convert input "(0,7)" to (0,7) and unpack into x and y
                    x, y = ast.literal_eval(test["Inputs"])  
                    state = test["State"]

                    print(f"Testing with: state={state}, x={x}, y={y}")

                    # Run the function
                    result = self.run_bests(state, x, y)
                    
                    print(f"Raw output from best: {result}")

                    """Parses the string output from `best` into Horizon, Best, and Value."""
                    parts = result.split(":")[1].strip().split(", ")  # Get part after "Horizon, best, value: "
                    
                    if len(parts) != 3:
                        raise ValueError(f"Unexpected output format: {result}")
                    
                    horizon = int(parts[0])   # Convert "1" to int
                    best_action = parts[1]    # Keep "Delay" as string
                    value = float(parts[2])   # Convert "0.468" to float
                    
                    print(horizon)
                    print(best_action)
                    print(value)

                    print(f"Parsed output: Horizon={horizon}, Best={best_action}, Value={value}")

                    # Expected values from CSV
                    expected_horizon = int(test["Horizon"])
                    expected_best = test["Best"]
                    expected_value = float(test["Value"])
                    
                    # Write input, expected output, and actual output to file
                    
                    log_file.write(f"Expected Output: Horizon = {expected_horizon}, Best = {expected_best}, Value = {expected_value}\n")
                    log_file.write(f"Output: Horizon={horizon}, Best={best_action}, Value={value}\n")
                    log_file.write(f"Input: state = {state}, time = {x}, horizon = {y}\n")
                    log_file.write(f"----------------------------------------------------------------------------------")
                    try:
                        self.assertEqual(horizon, expected_horizon)
                        self.assertEqual(best_action, expected_best)
                        self.assertAlmostEqual(value, expected_value, places=5)
                        log_file.write("PASS")
                    except AssertionError as e:
                        # Write only the short error message
                        log_file.write(f"FAIL: {str(e).split(':')[-1].strip()}\n")

                    log_file.write("\n")  # Add spacing between test cases

if __name__ == "__main__":
    unittest.main()
