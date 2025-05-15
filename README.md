### DISCLAIMER
This project aims at porting and translating the original source code presented in the research paper:
```
N. Botta m. fl., ”Responsibility Under Uncertainty: Which Climate Decisions Matter Most?”
Environmental Modelling & Assessment, yr. 28, s. 337–365, 2023. DOI: 10 . 1007 /
s10666-022-09867-w. URL: https://doi.org/10.1007/s10666-022-09867-w.
Original source code:https://gitlab.pik-potsdam.de/botta/papers/-/tree/master/2021.Responsibility%20under%20uncertainty:%20which%20climate%20decisions%20matter%20most%3F?ref_type=heads
```
Please note that the intellectual property rights to the code, including any modifications or alterations made by this project, remain with the original authors.
### Requirements
#### Alternative to Local Setup
To avoid setting up dependencies locally, you can use the provided Dockerfile in the root directory. Simply run the Docker application, build the Docker container, and start it.

If you wish to run the project using local dependencies, these are required:
##### Haskell Requirements
- `GHC 9.4.8 (or newer)`
- `Cabal version 3.10.0 or newer (version 3.12.1.0 is recommended)`
##### Python Requirements
- `Python 3.10 or newer`
- `numpy==1.26.0`
- `pandas>=2.0.0`
- `matplotlib~=3.8.0`
- `pytest>=8.3.5`
- `scipy>=1.15.3`

### Project Structure
The project is divided into two paradigms, one **Python** implementation and one **Haskell** implementation. Both paradigms reflect the same level of correctness and validation, and should be considered reflexive translations of each other.

### How-To-Run

##### Haskell:
1. From the root directory, run with cabal:
`cabal repl`
2. This opens a GHCI shell. From this shell session, load the module:
`:l haskell/src/Interface.hs`
3. From this module, user applicative functions such as `best`, `worst` and `meas` can be run with given inputs, example:
`runBest 0 1 DHU`
See documentation for further specifications on inputs, function theory and edge cases.

##### Python

1. Navigate to the python directory: `python`
2. Open a python shell session with the main.py argument: `python3 main.py`
4. Run the function with input parameters, example: `run_best(0, 1, State.DHU)`
