###############################################################################
# responsibility.py
# Translation of responsibility.lidr
###############################################################################

from typing import Callable, List  # Added Callable here
from theory import bi, bestExt, worstExt, val  # functions on policy sequences
from specification import State, StartDelay, isCommitted, isDisrupted
# You should also have a function to convert values to float:
def toDouble(x) -> float:
    return float(x)

###############################################################################
# Responsibility measures
###############################################################################
"""
We formulate and answer three questions (what does it mean for decisions to matter,
how can we measure this under uncertainty, and how can we compare decisions over time)
by extending the theory of sequential decision processes with a responsibility measure.
This measure is obtained in three steps:
  
  S1: Encoding the goal of decision making.
  S2: Comparing the best and conditional worst decisions.
  S3: Defining a normalized degree of responsibility.
"""

###############################################################################
# S1: Encoding goals of decision making
###############################################################################
# (Assume that the definitions for Theory.Val, reward, <+>, zero, (<=), etc.
# are provided elsewhere, e.g., in specification.py or theory.py.)

###############################################################################
# S2: Measuring how much decisions matter
###############################################################################
# A helper function: setInTo
def setInTo(f: Callable, a: State, b) -> Callable:
    """
    Returns a new function g such that:
      g(a) = b, and for any a' != a, g(a') = f(a').
    """
    def g(a_prime):
        if a_prime == a:
            return b
        else:
            return f(a_prime)
    return g

def best(t: int, n: int, x: State) -> str:
    """
    In Idris:
      best : (t, n : Nat) -> X t -> String
      best t Z x = "The horizon must be greater than zero!"
      best t (S m) x = let ps = bi (S t) m in
                       let p = bestExt ps in
                       let b = p x in
                       let vb = val (p :: ps) x in
                       "Horizon, best, value: " ++ show (S m) ++ ", " ++ show b ++ ", " ++ show vb
    """
    if n == 0:
        return "The horizon must be greater than zero!"
    else:
        ps = bi(t + 1, n - 1)  # backward induction: bi (S t) m, with m = n-1
        p = bestExt(ps)
        b = p(x)
        vb = val([p] + ps, x)
        return f"Horizon, best, value: {n}, {b}, {vb}"

def bests(t: int, ns: List[int], x: State) -> None:
    """
    In Idris:
      bests : (t : Nat) -> List Nat -> X t -> IO ()
      bests t Nil x = putStrLn "done!"
      bests t (n :: ns) x = do putStrLn (best t n x)
                              bests t ns x
    """
    for n in ns:
        print(best(t, n, x))
    print("done!")

###############################################################################
# S3: Responsibility measures (mMeas)
###############################################################################
def mMeas(t: int, n: int, x: State) -> float:
    """
    In Idris:
      mMeas : (t : Nat) -> (n : Nat) -> X t -> Double
      mMeas t Z x = 0.0
      mMeas t (S m) x = let ps = bi (S t) m in
                        let v  = toDouble (val (bestExt ps :: ps) x) in
                        let v' = toDouble (val (worstExt ps :: ps) x) in
                        if v == 0 then 0 else (v - v') / v
    """
    if n == 0:
        return 0.0
    else:
        ps = bi(t + 1, n - 1)
        best_policy = bestExt(ps)
        worst_policy = worstExt(ps)
        v = toDouble(val([best_policy] + ps, x))
        v_prime = toDouble(val([worst_policy] + ps, x))
        if v == 0:
            return 0.0
        else:
            return (v - v_prime) / v

###############################################################################
# Wrap-up
###############################################################################
"""
Through S1, S2, and S3 we have introduced a measure of how much decisions
under uncertainty matter. This measure is independent of a decision maker's
aims and is fair in the sense that all decisions are measured uniformly.
It provides a first example of a responsibility measure. In future work,
this measure can be generalized via a DSL for specifying goals.
"""