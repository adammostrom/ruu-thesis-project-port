from specification import Specification

if __name__ == "__main__":
    spec = Specification()
    result = spec.run_best(0, 1, "DHU")
    print(f"Result: {result}")