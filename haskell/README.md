# HASKELL SUBDIRECTORY
Directory Layout:
The `/haskell` subdirectory is organized into two directories:

- `src`
- `tests`


### src Directory

The `src directory` contains all the necessary components to run the program. The directory consists of different modules:
- `SDPTypes` 
The datatype of how an SDP is modelled in this project. Any SDP defines the functions declared in the datatype.

- `SDPCompute`
Contains the functions of the SDP translated from the originally implemented model from the Idris source code. These functions performs the computations of any new SDP implementation.

These two modules binds the model as presented and translated from the original source code.

In the `SDPCompute` module, users will also find the functions that implement memoization, which significantly improves the computational performance of the calculations and constitutes a development of the model.

##### Uncertainty Monad

The `Prob` monad models discrete uncertainty by wrapping possible outcomes with their probabilities. It allows to chain and compose stochastic computations using standard monadic operations (>>=), making it easy to build probabilistic logic. 

It represents  everything as a list of (value, probability) pairs. 

