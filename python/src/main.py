from specification import Specification


def run_best(spec, x, y, state):
    result = spec.best(x, y, state)
    print(result)

if __name__ == "__main__":
    spec = Specification()
    run_best(spec, 1, 2, "DHU")
