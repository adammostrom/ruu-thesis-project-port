# THEORY 

## SDPs:

Decision making problems where choises must be made over multiple steps, under uncertainty.

Theory contains 2 components:
- Specification : Defines how an SDP is structured mathematically
- Solution      : Uses verified backward induction to solve the SDP by working backwards from the end 

## UNCERTAINTY MONAD:

Monad = Type constructor
M : Type -> Type

Takes type `A` and produces a new type `M A` 

- Used to model uncertainty
- `M A` represents a finite probability distribution over values of type `A`
- Example: {(a1, 0.7), (a2, 0.3)}, a1 = 70% probability



# KEY COMPONENTS OF AN SDP
Defined by:
1) States (X t)- what the system looks like at each step
2) Controls (Y t x)- the actions available at a given state
3) State Transition Function (next t x y)- Describes how the state evolves based on the action taken

1) `X : (t : Nat) -> Type` = set of states at time step 1
2) `Y : (t : Nat) -> X t -> Type` = Available controls at step t in state x
3) `next : (t : Nat) -> (x : X t) -> Y t x -> M (X ( S t))` = Represents the uncertain transition from state x using control y at time t.

The result is wrapped in an uncertainty monad.
This monad ensures the uncertainty, and instead of a deterministic transition we can get probability distribution over a set of possible next states.

# DECISION PROCESS & DECISION PROBLEM
Decision Process = How system behaves given states, actions and transitions.
Decision Problem = Choosing the best actions over n steps to maximize some measure of success. (Called "Value Function")

- Policy         = Function that maps states to actions (Y t x)
- Value Function = Represents total reward obtained over n steps. (Reward can be reduction of pollution or economic savings).

Given a decision rule (policy), an initial state we can compute all possible sequences of states and their probabilities.

## THE CUSTOM <+> OPERATOR

Basically a standard numerical operator "+" but more flexible.
Can be used to "add" rewards in different regards, like applying a discount factor to rewards (Same reward today and in 10 years is more valuable today)

`<+> v1 v2 = v1 + gamma * v2`

Worst possible reward:

`(<+>) v1 v2 = min v1 v2`


Transitions are uncertain, hence rewards should reflect all possible next states.

`next t x y` returns an uncertainty monad which basically means that the next state is not deterministic.

If `reward t x y` is applied to each possible next state, we get a distribution over rewards `M Val` which represents the set of all possible rewards weighted by their probabilities.

To find the best policy, one must compare different sequences of actions in terms of their total rewards.

Requires: 

- measure function (`meas`)
This function maps the M Val to a single value
`meas : M Val -> Val `


Concept	Explanation
Val	                      = The type of reward values (e.g., money, cost, risk, utility).
reward t x y x'	          = The reward obtained by moving from x to x' using action y.
(<+>)	                  = Defines how rewards are combined (e.g., standard sum, discounted sum, worst-case).
M Val	                  = A probability distribution over possible rewards.
meas : M Val -> Val	      = A function that evaluates uncertain rewards, typically using expected value or risk-based measures.
(<=) : Val -> Val -> Type = A comparison function for different reward values.
TotalPreorder (<=)	      = Ensures that <= defines a consistent ranking of values.


## TOTAL PREORDER DEFINITION 

The implementation of the datatype TotalPreorder is essential for comparing values of type Val when deciding which policy sequence is better. 

## POLICIES AND POLICY SEQUENCES 
Policy          = function that maps states to actions (given state x, decide control y)

Policy : (t : Nat)   -> Type
Policy t = (x : X t) -> Y t x


Policy Sequence = a sequence of policies over multiple time steps.

`data PolicySeq : (t : Nat) -> (n : Nat) -> Type where 
    Nil : {t : Nat} -> PolicySeq t Z
    (::) : {t, n : Nat} -> Policy t -> PolicySeq (S t) n -> PolicySeq t (S n)`

The "::" means we can construct a sequence by prepending a policy to an existing sequence.



