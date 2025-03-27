# Importing relevant libraries.
import copy
from itertools import product

import numpy as np
import pandas as pd
from theory import TheoryInterface

# Global settings and setup of game.
n_players = 2


# Checking that no probabilities are negative.
def mkSimpleProb(pairs: list[tuple[str, float]]) -> dict[str, float]:
    dist: dict[str, float] = {}
    for st, pr in pairs:
        if pr >= 0:
            dist[st] = pr
    return dist 


class Player:
    def __init__(self, name: str, probs: dict[str, float], states: list[str], actions):
        self.name = name
        self.pC_Confess = probs["pC_Confess"]
        self.pR_Confess = probs["pR_Confess"]
        self.pC_Refuse = probs["pC_Refuse"]
        self.pR_Refuse = probs["pR_Refuse"]
        self.actions = actions
        self.last_action = None
        self.score = 0.0

    def choose_action(self, x: str) -> str:
        """For now, an action is chosen at random."""
        action = np.random.choice(self.actions(x))
        self.last_action = action
        return action


class Game(TheoryInterface):
    def __init__(self, players: list[Player], states: list[str]):
        self.players = players
        self.states = states
        self.current_state = "Start"
        self.zero = 0.0  # Default value of zero-length policy sequences.

    def nextFunc(self, t: int, x: str, ys: dict[str, str]) -> dict[str, float]:
        a1 = ys[self.players[0].name]
        a2 = ys[self.players[1].name]

        if a1 == "Confess" and a2 == "Confess":
            return mkSimpleProb(
                [
                    (
                        "CC",
                        self.players[0].pC_Confess * self.players[1].pC_Confess,
                    ),  # np.prod(player.pC_Confess for player in self.players)), something like this if arbitrary number of players.
                    ("CR", self.players[0].pC_Confess * self.players[1].pR_Confess),
                    ("RC", self.players[0].pR_Confess * self.players[1].pC_Confess),
                    ("RR", self.players[0].pR_Confess * self.players[1].pR_Confess),
                ]
            )
        elif a1 == "Confess" and a2 == "Refuse":
            return mkSimpleProb(
                [
                    ("CC", self.players[0].pC_Confess * self.players[1].pC_Refuse),
                    ("CR", self.players[0].pC_Confess * self.players[1].pR_Refuse),
                    ("RC", self.players[0].pR_Confess * self.players[1].pC_Refuse),
                    ("RR", self.players[0].pR_Confess * self.players[1].pR_Refuse),
                ]
            )
        elif a1 == "Refuse" and a2 == "Confess":
            return mkSimpleProb(
                [
                    ("CC", self.players[0].pC_Refuse * self.players[1].pC_Confess),
                    ("CR", self.players[0].pC_Refuse * self.players[1].pR_Confess),
                    ("RC", self.players[0].pR_Refuse * self.players[1].pC_Confess),
                    ("RR", self.players[0].pR_Refuse * self.players[1].pR_Confess),
                ]
            )
        elif a1 == "Refuse" and a2 == "Refuse":
            return mkSimpleProb(
                [
                    ("CC", self.players[0].pC_Refuse * self.players[1].pC_Refuse),
                    ("CR", self.players[0].pC_Refuse * self.players[1].pR_Refuse),
                    ("RC", self.players[0].pR_Refuse * self.players[1].pC_Refuse),
                    ("RR", self.players[0].pR_Refuse * self.players[1].pR_Refuse),
                ]
            )
        else:
            raise ValueError("Invalid action combination.")

    def reward(
        self, t: str, x: str, ys: dict[str, str], next_x: str
    ) -> dict[str, float]:
        rewards = {}
        if next_x == "CC":
            rewards[self.players[0].name] = 1
            rewards[self.players[1].name] = 1
        elif next_x == "CR":
            rewards[self.players[0].name] = 5
            rewards[self.players[1].name] = 0
        elif next_x == "RC":
            rewards[self.players[0].name] = 0
            rewards[self.players[1].name] = 5
        elif next_x == "RR":
            rewards[self.players[0].name] = 3
            rewards[self.players[1].name] = 3
        else:
            raise ValueError("Invalid next state.")
        return rewards

    # Function defining how to add rewards together.
    def add(self, a: float, b: float) -> dict[str, float]:
        if type(a) != dict or type(b) != dict:
            raise TypeError(
                f"Inputs must be of type 'dict', not '{type(a).__name__}' and '{type(b).__name__}'."
            )
        sum = {}
        for player in self.players:
            sum[player.name] = a[player.name] + b[player.name]
        return sum

    # Function for measuring a certain value.
    def meas(self, values: dict, pr: float) -> dict[str, float]:
        if type(values) != dict or type(pr) != float:
            raise TypeError(
                f"Inputs must be of type 'dict' and 'float', not '{type(values).__name__}' and '{type(pr).__name__}'."
            )
        measured = {}
        for player in self.players:
            measured[player.name] = values[player.name] * pr
        return measured  # Returns the expected value.

    # Computing the total expected value from a policy sequence when starting at time t in state x.
    def val(self, t: int, ps: dict[list[dict[str, str]]], x: str) -> dict[str, float]:
        if t < 0 or type(t) != int:
            raise ValueError(f"Invalid time step: '{t}' (must be positive integer).")
        if type(ps) != dict:
            raise TypeError(f"Invalid policy list, must be dictionary.")
        if x not in self.states:
            raise ValueError(f"Invalid state: '{x}'")

        values = {player.name: self.zero for player in self.players}
        ys = {}
        for player in self.players:
            if len(ps[player.name]) == 0:
                return values
            ys[player.name] = ps[player.name][0][x]
        m_next = self.nextFunc(t, x, ys)
        for x_prim, pr in m_next.items():
            new_vals = self.meas(
                self.add(
                    self.reward(t, x, ys, x_prim),
                    self.val(t + 1, {p: ps[p][1:] for p in ps}, x_prim),
                ),
                pr,
            )
            for player in self.players:
                values[player.name] += new_vals[player.name]

        return values

    def bestExt(
        self, t: int, ps_tail: dict[list[dict[str, str]]]
    ) -> dict[dict[str, str]]:
        policy = {}

        for player in self.players:
            policy[player.name] = {}

        for state in self.states:
            # Generate list of list of player actions in current state.
            all_actions = [player.actions(state) for player in self.players]

            # Initialize best action tracking
            best_values = {player.name: -np.inf for player in self.players}
            best_actions = {player.name: None for player in self.players}

            for action_combination in product(
                *all_actions
            ):  # Generates all possible player action combinations.
                ys = {}

                for player, action in zip(self.players, action_combination):
                    ys[player.name] = action  # Store the action for each player

                ps_prim = copy.deepcopy(ps_tail)
                for player in self.players:
                    ps_prim[player.name].insert(0, {state: ys[player.name]})

                # Compute the expected value for this action combination.
                value = self.val(t, ps_prim, state)

                # Update best action for each player based on their individual expected value
                for player in self.players:
                    if value[player.name] > best_values[player.name]:
                        best_values[player.name] = value[player.name]
                        best_actions[player.name] = ys[
                            player.name
                        ]  # Now ys[player.name] is correctly populated

            # Assign the best action found for this state.
            for player in self.players:
                policy[player.name][state] = best_actions[player.name]

        return policy

    # Builds an optimal policy sequence by recursively adding the best extension (starting from the end).
    def bi(self, t: int, n: int) -> list[dict[str, str]]:
        if n == 0:
            base = {player.name: [] for player in self.players}
            return base
        else:
            ps_tail = self.bi(t + 1, n - 1)
            p = self.bestExt(t, ps_tail)
            for player in self.players:
                ps_tail[player.name].insert(0, p[player.name])
            return ps_tail

    # For a given time step, state and decision horizon, returns the optimal action and the
    # expected value of the sequence it starts (assuming the rest of the sequence is optimal).
    def best(self, t: int, n: int, x: str) -> str:
        if n <= 0:
            raise ValueError("The horizon must be greater than zero!")
        ps = self.bi(t + 1, n - 1)
        p = self.bestExt(t, ps)
        b = {}
        for player in self.players:
            ps[player.name].insert(0, p[player.name])
            b[player.name] = p[player.name][x]
        vb = self.val(t, ps, x)
        return f"Horizon, best, value : {n}, {b}, {vb}"


def main():
    # Define probabilities.
    probs = {"pC_Confess": 1.0, "pR_Confess": 0.0, "pC_Refuse": 0.0, "pR_Refuse": 1.0}
    states = ["Start", "CC", "CR", "RC", "RR"]

    def actions(x):
        if x in states:
            return ["Confess", "Refuse"]
        else:
            raise ValueError(f"Invalid State: '{x}'.")

    # Create two players.
    player1 = Player("Player1", probs, states, actions)
    player2 = Player("Player2", probs, states, actions)
    players = [player1, player2]

    # Create the game instance.
    game = Game(players, states)

    # PLACEHOLDER.
    actions_chosen = {}
    for player in players:
        action = player.choose_action("Start")
        actions_chosen[player.name] = action
    print("Actions chosen:", actions_chosen)

    # Determine the outcome of the transition.
    outcome_distribution = game.nextFunc(0, game.current_state, actions_chosen)
    print("Outcome distribution:", outcome_distribution)

    # Compute rewards.
    rewards = game.reward(0, "Start", actions_chosen, "CC")
    print("Rewards:", rewards)

    ps1 = {
        "Player1": [
            {
                "Start": "Confess",
                "CC": "Confess",
                "CR": "Confess",
                "RC": "Confess",
                "RR": "Confess",
            },
            {
                "Start": "Confess",
                "CC": "Confess",
                "CR": "Confess",
                "RC": "Confess",
                "RR": "Confess",
            },
        ],
        "Player2": [
            {
                "Start": "Confess",
                "CC": "Confess",
                "CR": "Confess",
                "RC": "Confess",
                "RR": "Confess",
            },
            {
                "Start": "Confess",
                "CC": "Confess",
                "CR": "Confess",
                "RC": "Confess",
                "RR": "Confess",
            },
        ],
    }

    ps2 = {
        "Player1": [
            {
                "Start": "Refuse",
                "CC": "Refuse",
                "CR": "Refuse",
                "RC": "Refuse",
                "RR": "Refuse",
            },
            {
                "Start": "Refuse",
                "CC": "Refuse",
                "CR": "Refuse",
                "RC": "Refuse",
                "RR": "Refuse",
            },
        ],
        "Player2": [
            {
                "Start": "Refuse",
                "CC": "Refuse",
                "CR": "Refuse",
                "RC": "Refuse",
                "RR": "Refuse",
            },
            {
                "Start": "Refuse",
                "CC": "Refuse",
                "CR": "Refuse",
                "RC": "Refuse",
                "RR": "Refuse",
            },
        ],
    }

    ps3 = {"Player1": [], "Player2": []}

    values_ps1 = game.val(0, ps1, "CC")
    print("Values from ps1:", values_ps1)

    values_ps2 = game.val(0, ps2, "CC")
    print("Values from ps1:", values_ps2)

    best_extension = game.bestExt(0, ps3)
    # Convert dictionary to DataFrame
    df = pd.DataFrame.from_dict(best_extension, orient="index")
    print("Best extension:")
    print(df)

    # bi = game.bi(0, 3)
    # print("Backwards induction:", bi)

    best = game.best(0, 1, "CC")
    print("Best:", best)


if __name__ == "__main__":
    main()
