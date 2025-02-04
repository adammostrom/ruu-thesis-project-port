"""
=======================================================
 ORIGINAL IDRIS CODE SNIPPETS (in comments)
=======================================================

-------------------------------------------------------
 1. The 'M : Type -> Type' definition (the monad)
-------------------------------------------------------
> M : Type -> Type


-------------------------------------------------------
 2. States, Controls, and next
-------------------------------------------------------
> X : (t : Nat) -> Type
> Y : (t : Nat) -> X t -> Type
> next : (t : Nat) -> (x : X t) -> Y t x -> M (X (S t))


-------------------------------------------------------
 3. The Val type, reward, <+>, meas, <=, zero
-------------------------------------------------------
> Val       :  Type

> reward    :  (t : Nat) -> (x : X t) -> Y t x -> X (S t) -> Val
> (<+>)     :  Val -> Val -> Val
> meas      :  M Val -> Val
> (<=)      :  Val -> Val -> Type
> lteTP     :  TotalPreorder (<=)
> zero      :  Val


-------------------------------------------------------
 4. Policies
-------------------------------------------------------
> Policy : (t : Nat) -> Type
> Policy t = (x : X t) -> Y t x


-------------------------------------------------------
 5. PolicySeq
-------------------------------------------------------
> data PolicySeq : (t : Nat) -> (n : Nat) -> Type where
>   Nil   :  {t : Nat} -> PolicySeq t Z
>   (::)  :  {t, n : Nat} -> Policy t -> PolicySeq (S t) n -> PolicySeq t (S n)


-------------------------------------------------------
 6. The val function
-------------------------------------------------------
> val : Functor M => {t, n : Nat} -> PolicySeq t n -> X t -> Val
> val {t}  Nil      x  =  zero
> val {t} (p :: ps) x  =
>     let y   =  p x in
>     let mx' =  next t x y in
>     meas (map (reward t x y <++> val ps) mx')


-------------------------------------------------------
 7. OptPolicySeq
-------------------------------------------------------
> OptPolicySeq  :  Functor M => {t, n : Nat} -> PolicySeq t n -> Type
> OptPolicySeq ps  =  (ps' : PolicySeq t n) -> (x : X t) -> val ps' x <= val ps x


-------------------------------------------------------
 8. BestExt and Bellman
-------------------------------------------------------
> BestExt  :  Functor M => {t, n : Nat} -> PolicySeq (S t) n -> Policy t -> Type
> BestExt ps p  =  (p' : Policy t) -> (x : X t) -> val (p' :: ps) x <= val (p :: ps) x

> Bellman  :  Functor M => {t, n : Nat} ->
>             (ps   :  PolicySeq (S t) n) -> OptPolicySeq ps ->
>             (p    :  Policy t)          -> BestExt ps p ->
>             OptPolicySeq (p :: ps)
"""

# =======================================================
# PYTHON TRANSLATION
# =======================================================

from typing import Any, Dict, Callable, List, TypeVar

###############################################################################
# 1. M : Type -> Type
###############################################################################
"""
In Python, we model the monad M[A] with a Dict[A, float], 
i.e. a dictionary from A to probabilities or weights.

But we keep it abstract here, with placeholders for 'map', etc.
"""
A = TypeVar('A')  # generic type
M = Dict[A, float]  # A dictionary from A to a float (e.g. probability)

def m_map(fn: Callable[[A], Any], m: M[A]) -> M[Any]:
    """
    This function would correspond to the 'map' that applies `fn` to each key 
    in the monad M. 
    For now, we just show the structure and do not assume how collisions 
    or probabilities are handled. 
    """
    # Placeholder code, summing probabilities if keys collide, for example:
    out: Dict[Any, float] = {}
    for (key, prob) in m.items():
        new_key = fn(key)
        out[new_key] = out.get(new_key, 0.0) + prob
    return out


###############################################################################
# 2. X, Y, next
###############################################################################
"""
X : (t : Nat) -> Type
Y : (t : Nat) -> X t -> Type
next : (t : Nat) -> (x : X t) -> Y t x -> M (X (S t))

We do not fill them with real data, we just show placeholders.
"""

# We'll represent 'State' and 'Control' as type variables or stubs:
State = TypeVar('State')
Control = TypeVar('Control')

def Y(t: int, x: State) -> List[Control]:
    """
    Return the list (or set) of possible controls (Y t x).
    Placeholder - user to define how controls depend on t, x.
    """
    raise NotImplementedError("Define Y(t, x) for your application.")

def next_state_dist(t: int, x: State, y: Control) -> M[State]:
    """
    The 'next' function: returns a monadic distribution of next states 
    given the current state x and control y at time t.
    Placeholder - user to define transitions.
    """
    raise NotImplementedError("Define the transition distribution for your application.")


###############################################################################
# 3. Val, reward, <+>, meas, <=, zero
###############################################################################
"""
Val : Type

reward : (t : Nat) -> (x : X t) -> Y t x -> X (S t) -> Val
(<+>)  : Val -> Val -> Val
meas   : M Val -> Val
(<=)   : Val -> Val -> Type
zero   : Val
"""

Val = TypeVar('Val')  # We keep it abstract

def reward(t: int, x: State, y: Control, x_next: State) -> Val:
    """
    Placeholder: user defines how to compute reward from the transition 
    (x -> x_next) via control y at time t.
    """
    raise NotImplementedError("Define reward(t, x, y, x_next).")

def val_add(v1: Val, v2: Val) -> Val:
    """
    Corresponds to (<+>) in Idris. 
    For numeric Val, it's an addition. For more complex, define your logic.
    """
    raise NotImplementedError("Define how to add two Val values (<+>).")

def meas(mval: M[Val]) -> Val:
    """
    The 'meas' function collapses an M Val into a single Val, e.g. 
    expected value or worst-case, etc.
    """
    raise NotImplementedError("Define measure (meas) of M[Val].")

def val_le(v1: Val, v2: Val) -> bool:
    """
    The (<=) relation for Val. Must be a total preorder if you want 
    to replicate lteTP from Idris.
    """
    raise NotImplementedError("Define <= on Val.")

# The 'zero' value: a special "reference" or "neutral" element in Val.
def zero() -> Val:
    raise NotImplementedError("Define the zero Val.")


###############################################################################
# 4. Policy
###############################################################################
"""
Policy : (t : Nat) -> Type
Policy t = (x : X t) -> Y t x

In Python, let's define a 'Policy' as a callable from State -> Control.
"""

Policy = Callable[[State], Control]


###############################################################################
# 5. PolicySeq
###############################################################################
"""
data PolicySeq : (t : Nat) -> (n : Nat) -> Type where
  Nil   : ...
  (::)  : ...

We'll model it as a simple list of Policy in Python.
"""

PolicySeq = List[Policy]


###############################################################################
# 6. val function
###############################################################################
"""
Idris:
> val {t}  Nil      x  =  zero
> val {t} (p :: ps) x  =
>   let y   = p x in
>   let mx' = next t x y in
>   meas (map (reward t x y <++> val ps) mx')

In Python, we define a recursive function that does the same.
"""

def val_func(t: int, n: int, ps: PolicySeq, x: State) -> Val:
    """
    If ps is empty (Nil), return zero.
    Otherwise, take the first policy p, compute y = p(x), then next_state_dist,
    map reward+val, and then meas it.
    
    'n' can help track how many steps remain, if needed, or can be ignored 
    if your data structure is just length of ps.
    """
    if len(ps) == 0:
        return zero()
    else:
        p = ps[0]
        rest = ps[1:]
        y = p(x)
        mx_prime = next_state_dist(t, x, y)

        # We map (reward t x y x_next) <+> val_func(t+1, rest, x_next) 
        def map_fn(x_next: State) -> Val:
            r = reward(t, x, y, x_next)
            future_val = val_func(t + 1, n - 1, rest, x_next)
            return val_add(r, future_val)

        mapped = m_map(map_fn, mx_prime)  # M[Val]
        return meas(mapped)


###############################################################################
# 7. OptPolicySeq
###############################################################################
"""
> OptPolicySeq ps = (ps' : PolicySeq t n) -> (x : X t) -> val ps' x <= val ps x
In Python, we cannot define that as a type, but we define a function to check it.
"""

def is_opt_policy_seq(t: int, n: int, candidate: PolicySeq,
                      all_candidates: List[PolicySeq],
                      states: List[State]) -> bool:
    """
    Checks if for all ps' of same length in all_candidates, 
    and all x in states, val(ps', x) <= val(candidate, x).
    """
    for ps_prime in all_candidates:
        if len(ps_prime) != len(candidate):
            continue
        for x in states:
            lhs = val_func(t, n, ps_prime, x)
            rhs = val_func(t, n, candidate, x)
            if not val_le(lhs, rhs):
                return False
    return True


###############################################################################
# 8. BestExt and Bellman
###############################################################################
"""
> BestExt ps p  =  (p' : Policy t) -> (x : X t) -> val (p' :: ps) x <= val (p :: ps) x
> Bellman ps (OptPolicySeq ps) p (BestExt ps p) => OptPolicySeq (p :: ps)

We show how to define a check for BestExt in Python. 
Bellman is a theorem in Idris, so we'll just illustrate a structure.
"""

def is_best_extension(t: int, n: int, tail_ps: PolicySeq, p_candidate: Policy,
                      single_step_policies: List[Policy],
                      states: List[State]) -> bool:
    """
    For each p' in single_step_policies and each x in states,
    check val([p'] + tail_ps, x) <= val([p_candidate] + tail_ps, x).
    """
    for p_prime in single_step_policies:
        for x in states:
            lhs = val_func(t, n, [p_prime] + tail_ps, x)
            rhs = val_func(t, n, [p_candidate] + tail_ps, x)
            if not val_le(lhs, rhs):
                return False
    return True

def bellman(ps_tail: PolicySeq,
            ps_tail_is_optimal: bool,  # we can't pass a 'proof', just a bool
            p: Policy,
            p_is_best_ext: bool       # also a bool 
           ) -> bool:
    """
    In Idris, Bellman is a proof that (p :: ps_tail) is optimal 
    if ps_tail is optimal and p is a best extension.
    Here we can only represent it as a placeholder function 
    returning the conclusion if those are True.
    """
    # If tail is optimal and p is a best extension,
    # then (p :: ps_tail) is also optimal. (In theory, returns True if so.)
    # In an actual formal system, you'd do a proof. 
    # We'll just do a naive placeholder:
    return ps_tail_is_optimal and p_is_best_ext


###############################################################################
# (Optional) 9. bestExt function and backward induction
###############################################################################
"""
You can define a function that enumerates all possible single-step policies 
and picks one that is the best extension. 
We'll keep it as a placeholder to avoid "inventing" data.
"""

def best_ext(t: int, n: int, ps_tail: PolicySeq) -> Policy:
    """
    A function to return a single-step policy that is a best extension of ps_tail.
    Because we do not want to 'make up' data, we just raise NotImplementedError.
    """
    raise NotImplementedError("User must define how to pick a best extension.")

def bi(t: int, n: int) -> PolicySeq:
    """
    The backward induction function from the Idris code:
    bi t 0 = Nil
    bi t (S n) = let ps = bi (S t, n) in bestExt ps :: ps
    """
    if n == 0:
        return []
    else:
        ps_tail = bi(t + 1, n - 1)
        p_star = best_ext(t, n, ps_tail)
        return [p_star] + ps_tail


###############################################################################
# END OF TRANSLATION
###############################################################################
"""
Usage notes:
- All functions are placeholders or stubs, raising NotImplementedError 
  for the parts you need to define yourself: e.g., Y(t, x), next_state_dist(...), 
  reward(...), val_add(...), meas(...), val_le(...), best_ext(...).
- Once you define these details, the structure will match the Idris code closely.
- The code above won't run as-is until those stubs are implemented.
"""
