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

This directory contains the translation of the main component presented in the *Responsibility Under Uncertainty* research paper, located in `theory.py`. Unlike the original version, this file has been refactored into an abstract class using Python’s `abc module` (see docs). The abstract class is named `SDP` and defines the following required functions: `next`, `reward`, `actions` and `states`.


    - `reward` 
    The reward function yields the reward of selecting control `y` in state `x` when the next state is `x'`. Controls (or Actions) can represent resource usage or decisions, and rewards may depend on both the current and next state. In this implementation, rewards are combined using standard addition, but other combination rules can be used to model different applications.

    - `next`
    The transition function `next` maps a state `x` and control `y` at decision step `t` to dict[State, float] datastructure,  representing the possible next states, where the float number is the probability. 

    - `actions`
    The actions function returns available actions as each timestep given a state. 

    - `states`
    The states function returns a list of all available states for a given timestep.

Any new SDP can be implemented by inheriting from the SDP class in `theory.py`or `theoryMemoization.py` (recommended) and correctly defining the required functions. This structure allows the new class to use the computational logic from SDP, while specifying its own behavior for those functions.

**Group 12** has extended the original model by introducing memoization, significantly improving computational performance. This implementation is in `theoryMemoization.py`. New implementations of the SDP model can inherit from either `theory.py` or `theoryMemoization.py`.


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
The model uses discretized state spaces with configurable granularity (e,c) and implements probabilistic transitions using a normal distribution model with action-dependent drifts.

**Labyrinth**
A visual introduction to SDPs, where sequential decision-making is illustrated using mazes solved by the model.

**MatterMostPareto**
Contains the SDP presented as MatterMost in the original paper, but with a "split" SDP with different reward function. The reward results are compared to find a pareto front

**NumberLine**
A simple, educational implementation meant to be followed “by hand” to better understand step-by-step SDP computations.

Note: To run any implementation, from the root directory, use:
    `> make python-run`


#### How to solve an SDP case:

There are several cases implemented in the python module. By running `make python run` you will be prompted to select an implementation.
For the Matter Most case, the documentation and the original source code paper contains extensive information on the case and is recommended to read if the user wants more in depth knowledge about the case and how the code computes the SDP.

For users that want to explore by doing, we recommend running the Matter Most gase, and consider the scenarios:

Run the program:
From the root folder, type:

`make python-run`

You’ll get a prompt to select an SDP implementation (e.g., MatterMost, NumberLine, Labyrinth).

Pick a scenario & state:
For example, in MatterMost, try:

`best(0, 1, State.DHU)`

This asks: “What’s the best action at time 0 with horizon 1 in state DHU?”

Understand output:
Example output: "Delay, 0.468" means delaying the green transition has an expected reward of 0.468.

Explore time horizons:
Run for different horizons (1 to N) to see how optimal decisions change over time:

`bests(0, [1..8], State.DHU)`

Usually, longer horizons favor starting transition, shorter favor delay.


#### Creating Your Own SDP case

1. Start with `SpecificationTemplate.py` as a base. Rename the default class **Specification** to match your new SDP.
2. Define the relevant State and Action Enums.
3. Implement the required functions: `next`, `reward`, and `states`.
4. Name your file and class appropriately. The file should be named like `ExampleSDP.py`, and the class inside should be named **Example(SDP)**, inheriting from SDP.

To test your implementation, run the test suite via:
`> make python-test`
Then select your SDP from the test menu.

Note: The test configuration (`testconfig.py`) expects that:

1. Your file is placed in src/implementations/

2. The class name matches the filename, minus the SDP suffix. (e.g. `ExampleSDP.py` must contain class **Example(SDP)**)

### tests Directory
Note: `pytest` is required for the tests to work properly. See repository root README or `requirements.txt`.

The test directory contains a set of files "testing suites" that reflect both specific testing of implementations (`test_MatterMost`, `test_numberLineSDP`) as well as a generic test suite that contains a set of property tests which capsulate the intended and required behaviour of a correctly implemented SDP inheriting the model.

For more information on property tests, users are encouraged to navigate to: https://en.wikipedia.org/wiki/Property_testing.

With properties we can assert certain behaviours of a program independent of the specifications. An important property of any SDP is that the *backwards induction* (found in: `/src/theory.py` & `/src/theoryMemoization.py`) returns an optimal sequence if policies. This property is formulated as a test in the `tests/test_properties.py`. 

For each state at time t, we compare the value of following the policy sequence returned by `bi(t, n)` to the value of following an alternative policy where we take a different action at t and follow the same tail. 

If `bi` is optimal, its value must be greater than or equal to the value of any such alternative.

Since `bi` constructs the policy sequence by recursively selecting the best possible action at each time step (with respect to the optimal tail), this test asserts the Bellman optimality condition at each state and time step. If this condition holds for all states, we conclude that `bi` returns an optimal policy sequence.

Since `bi` builds the sequence recursively by always choosing the best local action, the whole sequence is globally optimal.

Computing the best policy sequence using backwards induction.
```python
    ps = bi(t, n)
```
For each state s at time t, compute the value of following the optimal policy:
```python
    V_opt = val(t, ps, s)
```
We try every action `a` from a given state and timestep. With this action, we make a new policy sequence and give it `a`

```python
    for a in actions(t, s):
        test_policy = [{s: (a, _)}] + ps[1:]
        V_test = val(t, test_policy, s)
        assert V_test <= V_opt
```

This directly tests the condition that no other action at t could yield a higher value — if any V_test > V_opt, then the supposedly optimal policy is not truly optimal.

To run this specific test, type:
 `> pytest python/tests/test_properties.py::test_bi_policy_optimality`

Users are encouraged to look through the `tests/test_properties.py` and `tests/test_propertiesMemo.py` to explore the different properties an SDP should hold.


#### misc

The folder also contains a subfolder `/exhaustive_test_results` which contains a large suite of tests targeting the "surface" or "user-interaction" functions. The folder contains each subfolder for each function, and in respective subfolder a csv file `<func>_test_cases.csv` which contains the results from running the function given a subset of inputs from the original source code written in Idris. These results are then compared using the respective testScripts with the actual function implemented for the `MatterMost` SDP in `python/src/implementations` and comparing the result.