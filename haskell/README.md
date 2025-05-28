# HASKELL SUBDIRECTORY
Directory Layout:
The `/haskell` subdirectory is organized into two directories:

- `src`
- `tests`


### src Directory

The `src directory` contains all the necessary components to run the program. The directory consists of different modules:

- `SDPTypes` 
The datatype of how an SDP is modelled in this project. Any SDP defines the functions declared in the datatype. This datatype wraps the model to an SDP, and declares 4 functions that any SDP using this datatype need to define:

    - `reward` 
    The reward function yields the reward of selecting control `y` in state `x` when the next state is `x'`. For this datatype, `reward` returns a type `Val`, which here is defined as a `Double`. Controls (or Actions) can represent resource usage or decisions, and rewards may depend on both the current and next state. In this implementation, rewards are combined using standard addition, but other combination rules can be used to model different applications.

    - `next`
    The transition function `next` maps a state `x` and control `y` at decision step `t` to a structure `Prob x` representing the possible next states. The monad `Prob` captures uncertainty in transitions, allowing us to model stochastic or non-deterministic outcomes.

    - `actions`
    The actions function returns available actions as each timestep given a state. 

    - `states`
    The states function returns a list of all available states for a given timestep.

- `SDPCompute`
Contains the functions of the SDP translated from the originally implemented model from the Idris source code. These functions performs the computations of any new SDP implementation.

These two modules binds the model as presented and translated from the original source code.

In the `SDPCompute` module, users will also find the functions that implement memoization, which significantly improves the computational performance of the calculations and constitutes a development of the model.

##### Uncertainty Monad (Prob)

The `Prob` monad models discrete uncertainty by wrapping possible outcomes with their probabilities. It allows to chain and compose stochastic computations using standard monadic operations (>>=), making it easy to build probabilistic logic. 

It represents  everything as a list of (value, probability) pairs. 

##### GHGCase, GHGCaseParam

These modules outlines the original SDP case presented in the research paper *Responsibility Under Uncertainty*. The GHGCaseParams contains the paramters for different probabilites as presented in the paper. The GHGCase defines the functions `next`, `reward`, `states`, `actions` by using the datatype `SDP` found in `SDPTypes`.

##### AdvancedStates, AdvancedProb

These modules constitutes the development effort of group 12 by expanding upon the case presented by the authors of the *Responsibility Under Uncertainty* paper. 
The model uses discretized state spaces with configurable granularity (e,c) and implements probabilistic transitions using a normal distribution model with action-dependent drifts.

##### Interface Modules

These modules are the entry point for the different implementations, as of 2025-05-22 this serves as the interaction space. Any new implementation is encouraged to either copy the `Interface.hs` module and swap out the sdp to interact with, or implement their own. Preferably naming it with "InterfaceXY" where XY is up to the author, this way the entry point (Main) can find the modules.

#### Interface Modules

These modules (named InterfaceXX.hs) are intended to be the user interaction point and would contain descriptive information of how to run the functions that are exposed.

**InterfaceGHGCase**

Interaction point for the GHGCase as originally presented in *"Responsibility under Uncertainty - Which Climate Decision matter most?*.

The user will be prompted with the exposed functions implemented by the case module. 


**InterfaceAdvancedStates**

This module contains the interaction point for the "Advanced States" expansion on the GHG case.

#### How to solve an SDP case:

In the haskell module, we have implemented the original case as presented in the paper: GHGCase. By running `make haskell-run` you will be prompted to select an implementation.
For the Matter Most case, the documentation and the original source code paper contains extensive information on the case and is recommended to read if the user wants more in depth knowledge about the case and how the code computes the SDP.

For users that want to explore by doing, we recommend running the Matter Most gase, and consider the scenarios:

Run the program:
From the root folder, type:

`make haskell-run`

You’ll get a prompt to select an SDP implementation.

Select an implementation by loading that module. Information on how to do this should be given as you run the `make haskell-run`

After you have loaded the GHGCase Interface, try running a function like:
`best 0 1 DHU`

This asks: “What’s the best action at time 0 with horizon 1 in state DHU?”

Understand output:
Example output: "Delay, 0.468" means delaying the green transition has an expected reward of 0.468.

Explore time horizons:
Run for different horizons (1 to N) to see how optimal decisions change over time:

`[best 0 n DHU | n <- [1..8]]`

Usually, longer horizons favor starting transition, shorter favor delay.

We recommend reading the documentation to understand what the functions does and how to interpret them, while exploring and experimenting with the framework.

#### Creating Your Own SDP case

1. Start by going through the SDPCompute module and understanding the datastructure of SDPTypes.
2. Define the relevant State and Action Enums.
3. Create a new file (module), naming it appropriately, and start by creating the core functions (next, actions, states, reward).
4. Remember to import the SDPTypes, and create a local "instance" of the SDP, like so:

```haskell
-- | An SDP instance example
sdpExample :: SDP State Action
sdpExample = SDP reward next actions states

```
5. After you have created the file, you can always run it directly via `cabal repl` and loading it manually. You are however recommended to create an "InstanceXX.hs" file, where you import your sdp, create wrapper functions for the parts you wish to expose, and allows the interface to serve as an interactive/testing area for the SDP.

To test your implementation, navigate to the "`Test_SDP.hs`" file, and simply swap out the current case with yours. There should be a line to simply exchange the sdp to test:

```haskell
-- | Adjust this import based on the SDP to test. 
import GHGCase as Case
```

Run it manually either via `cabal repl` and load the test file, and simply run "`testAll`", or, from root run `make haskell-test` and the testfile should appear for your selection.

### Tests Directory

The test directory contains the property based tests that uses quickCheck. For more information on quickCheck and property based testing, see: https://hackage.haskell.org/package/QuickCheck.

With properties, we can assert certain behaviors of a program independently of its implementation. An important property of any Stochastic Dynamic Program (SDP) is that backwards induction (implemented in /src/SDPCompute.hs as bi) returns an optimal policy sequence.

This is tested in `tests/Test_SDP.hs` via a property called `prop_biOptimal`.

For a given time t and horizon n, the function `bi t n` returns a policy sequence. For each state x at time t, the test compares:

- the value of following this optimal policy:

`V_opt = val t ps x`

- to the value of following an alternative policy ps', which differs only in the action taken at time t (but keeps the same tail for t+1 onward).

If bi is truly optimal, then:

`val t ps' x <= val t ps x`

for all such alternatives.

This directly checks the Bellman optimality condition — that choosing the locally best action at time t, followed by the optimal tail, yields the highest value for all reachable states.

Because bi builds the policy recursively, selecting the best local action at each step, and assuming correct implementation of val, this test guarantees the global optimality of the resulting sequence.


This directly tests the condition that no other action at t could yield a higher value — if any V_test > V_opt, then the supposedly optimal policy is not truly optimal.

To run this specific test, type:
 `> make haskell-run`
 `> :l haskell/tests/Test_SDP.hs`
 `> quickCheck prop_biOptimal`

Users are encouraged to look through the `tests/Test_SDP.hs`to explore the different properties an SDP should hold.


#### misc

The folder also contains the exhaustive tests targeting the "surface" or "user-interaction" functions. The folder contains a txt file `<func>_test_data.txt` which contains the results from running the function given a subset of inputs from the original source code written in Idris. These results are then compared using the respective test modules with the actual function implemented for the `MatterMost` SDP and comparing the result.