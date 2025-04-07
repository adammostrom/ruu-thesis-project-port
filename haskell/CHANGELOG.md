## 0.1.0.1 -- 2025-03-14
* Refactored `val` function to return a Double (Val) instead of a Monadic `Prob Val` 
* Refactored `CoreComputation` into 3 separate files for clarity and alignment with original source code: ("Theory.hs", "Specification.hs", "Responsibility.hs")
* Intent is to run the program from Main (Main serves as wrapper), and NOT via Responsibility as in the source code.