### DISCLAIMER
This project aims at porting and translating the original source code presented in the research paper:
```
N. Botta m. fl., ”Responsibility Under Uncertainty: Which Climate Decisions Matter Most?”
Environmental Modelling & Assessment, yr. 28, s. 337–365, 2023. DOI: 10 . 1007 /
s10666-022-09867-w. URL: https://doi.org/10.1007/s10666-022-09867-w.
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
### Project Structure
The project is divided into two paradigms, one **Python** implementation and one **Haskell** implementation. Both paradigms reflect the same level of correctness and validation, and should be considered reflexive translations of each other.

### How-To-Run

##### Haskell:
1. From the root directory, run with cabal:
`cabal repl`
2. This opens a GHCI shell. From this shell session, load the module:
`:l haskell/src/CoreCompuation.hs`
3. From this module, user applicative functions such as `best`, `worst` and `meas` can be run with given inputs, example:
`best 0 1 DHU`
See documentation for further specifications on inputs, function theory and edge cases.

##### Python

1. Navigate to the python directory: `python/src`
2. Open a python shell session: `python`
3. Import the *mainFile*: `import mainFile.py`
4. Run the function with input parameters, example: `mainFile.run_best(0,1,"DHU")`
