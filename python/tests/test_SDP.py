from enum import Enum, auto

from src.application.theory import SDP, Action

#==================== Test SDP Implementation ====================

"""Define the possible states for the SDP"""
class State(Enum):
    S1 = auto()
    S2 = auto()
    S3 = auto()
    S4 = auto()
    
"""Define possible actions for the SDP"""   
class Action(Enum):
    A1 = auto()
    A2 = auto()
    A3 = auto()
    A4 = auto()

"""Arbitrary implementation of the SDP class for testing"""
class TestSDP(SDP):

    @property
    def states(self):
        return list(State)
    
    """Define action behavior based on state, arbitrary."""
    def actions(self, t: int, x: State) -> list[Action] | list[None]:
        if x in [State.S1, State.S2]:
            return [Action.A1, Action.A2]
        elif x in [State.S3, State.S4]:
            return [Action.A3, Action.A4]
    """ 
    Arbitraily implemented next function. 
    The property tests make no assertions on the static values, only on the behaviour of the function.
    """
    def nextFunc(self, t: int, x: State, y: Action) -> dict[State, float]:
        if t == 0:
                if x == State.S1 or x == State.S2:
                    if y == Action.A1:
                        return self.mkSimpleProb([(State.S1, 0.6), (State.S2, 0.4)])
                    elif y == Action.A2:
                        return self.mkSimpleProb([(State.S1, 0.5), (State.S2, 0.5)])
                    elif y == Action.A3:
                        return self.mkSimpleProb([(State.S3, 0.5), (State.S2, 0.5)])
                    elif y == Action.A4:
                        return self.mkSimpleProb([(State.S1, 0.25), (State.S2, 0.75)])
                elif x == State.S3 or x == State.S4:
                    if y == Action.A1:
                        return self.mkSimpleProb([(State.S3, 0.9), (State.S4, 0.1)])
                    if y == Action.A3 or y == Action.A2:
                        return self.mkSimpleProb([(State.S3, 0.4), (State.S4, 0.6)])
                    elif y == Action.A4:
                        return self.mkSimpleProb([(State.S3, 0.7), (State.S4, 0.3)])
        elif t > 0:
            if x == State.S1 or x == State.S2:
                if y == Action.A1:
                    return self.mkSimpleProb([(State.S1, 0.7), (State.S2, 0.3)])
                elif y == Action.A2:
                    return self.mkSimpleProb([(State.S1, 0.6), (State.S2, 0.4)])
                elif y == Action.A3:
                    return self.mkSimpleProb([(State.S1, 0.8), (State.S4, 0.2)])
                elif y == Action.A4:
                    return self.mkSimpleProb([(State.S2, 0.76), (State.S4, 0.24)])
            elif x == State.S3 or x == State.S4:
                if y == Action.A1:
                    return self.mkSimpleProb([(State.S2, 0.3), (State.S1, 0.7)])
                elif y == Action.A2:
                    return self.mkSimpleProb([(State.S3, 0.45), (State.S1, 0.55)])
                elif y == Action.A3:
                    return self.mkSimpleProb([(State.S3, 0.6), (State.S4, 0.4)])
                elif y == Action.A4:
                    return self.mkSimpleProb([(State.S3, 0.5), (State.S4, 0.5)])
            
    def reward(self, t: int, x: State, y: Action, next_x: State) -> float:
        if next_x in [State.S1, State.S2]: return 1.0
        else: return 0.0