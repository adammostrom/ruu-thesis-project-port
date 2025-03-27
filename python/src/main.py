from numberLineSDP import NumberLineSDP
from specification import Specification
from theory import TheoryInterface


def run_best(spec, x, y, state):
    result = spec.best(x, y, state)
    print(result)

if __name__ == "__main__":
    pass
