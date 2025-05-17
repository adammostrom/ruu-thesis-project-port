# PYTHON SUBDIRECTORY
Directory Layout

The `/python` subdirectory is organized into two directories:

- `src`
- `tests`

### src Directory

The `src directory` contains all the necessary components to run the program. The model is divided into submodules (subfolders) based on usage and component purpose:

- `/application`: Contains the main program and its components.

- `/implementations`: Contains implementations of the SDP model defined in the application folder.
- `/utils`: Contains error checking module as well as various computational functions used by the SDP model.

#### application Directory

This directory contains the translation of the main component presented in the *Responsibility Under Uncertainty* research paper, located in `theory.py`. Unlike the original version, this file has been refactored into an abstract class using Python’s `abc module` (see docs). The abstract class is named `SDP` and defines the following required methods: `next`, `reward`, and `states`.

Any new SDP can be implemented by inheriting from the SDP class in `theory.py`or `theoryMemoization.py` (recommended) and correctly defining the required methods. This structure allows the new class to use the computational logic from SDP, while specifying its own behavior for those methods.

**Group 12** has extended the original model by introducing memoization, significantly improving computational performance. This implementation is in `theoryMemoization.py`. New**SDPs can choos**e to inherit from either `theory.py` or `theoryMemoization.py`.


TODO: multistakeholder theory


#### utils Directory
The utils directory contains components used by the SDP, mainly:
- `errorChecks`
Self explanatory. Checking and catching bad inputs and wrong computations made by any SDP.
- `geenratePlots`
Component mainly used by the Pareto implementation.  (TODO?)
- `mathOperations`
Math operations separated away from the SDP due to Separation of Concerns. Can be used for other purposes or other custom SDP theories.


#### implementations Directory

This directory includes the “MatterMost” implementation, as originally published in the *Responsibility Under Uncertainty* paper, as well as other implementations developed by **Group 12**:

**AdvancedStates**
A new approach to state management. (TODO: expand description)

**Labyrinth**
A visual introduction to SDPs, where sequential decision-making is illustrated using mazes solved by the model.

**MatterMostPareto**
A Pareto-optimized implementation. (TODO: expand description)

**NumberLine**
A simple, educational implementation meant to be followed “by hand” to better understand step-by-step SDP computations.

Note: To run any implementation, from the root directory, use:
    `> make python-run`

#### Creating Your Own SDP

1. Start with `SpecificationTemplate.py` as a base. Rename the default class **Specification** to match your new SDP.
2. Define the relevant State and Action Enums.
3. Implement the required methods: `next`, `reward`, and `states`.
4. Name your file and class appropriately. The file should be named like `ExampleSDP.py`, and the class inside should be named **Example(SDP)**, inheriting from SDP.

To test your implementation, run the test suite via:
`> make python-test`
Then select your SDP from the test menu.

Note: The test configuration (`testconfig.py`) expects that:

1. Your file is placed in src/implementations/

2. The class name matches the filename, minus the SDP suffix. (e.g. `ExampleSDP.py` must contain class **Example(SDP)**)

### tests Directory

The test directory contains a set of files "testing suites" that reflect both specific testing of implementations (`test_MatterMost`, `test_numberLineSDP`) as well as a generic test suite that contains a set of property tests which capsulate the intended and required behaviour of a correctly implemented SDP inheriting the model.

For more information on property tests, users are encouraged to navigate to: https://en.wikipedia.org/wiki/Property_testing.

With properties we can assert certain behaviours of a program independent of the specifications. An important property of any SDP is that the *backwards induction* (found in: `/src/theory`) returns an optimal 