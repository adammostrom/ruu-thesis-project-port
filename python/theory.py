from enum import Enum
from typing import Dict, List, Callable, TypeVar, Generic
from functools import reduce

# Define State and Control as Enums for type safety
class State(Enum):
    DHU = "DHU"
    DHC = "DHC"
    SLC = "SLC"
    # Add other states as needed

class Control(Enum):
    Start = "Start"
    Delay = "Delay"
    # Add other controls as needed

Val = float  # Reward type (simplified to float)
M = Dict[State, float]  # Monad: probability distribution over states