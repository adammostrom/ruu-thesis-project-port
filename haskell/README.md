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

**InterfaceGHGCase**


**InterfaceAdvancedStates**

