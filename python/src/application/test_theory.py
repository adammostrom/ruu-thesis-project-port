import unittest
from unittest.mock import MagicMock

from theory import SDP, State


class TestSDP(unittest.TestCase):
    def setUp(self):
        # Create a mock subclass of SDP to test the abstract methods
        class MockSDP(SDP):
            @property
            def states(self):
                return [State("A"), State("B")]

            def actions(self, t, x):
                return []

            def nextFunc(self, t, x, y):
                return {}

            def reward(self, t, x, y, next_x):
                return 0.0

        self.sdp = MockSDP()

    def test_mkSimpleProb_valid_probabilities(self):
        pairs = [(State("A"), 0.6), (State("B"), 0.4)]
        result = self.sdp.mkSimpleProb(pairs)
        self.assertEqual(result, {State("A"): 0.6, State("B"): 0.4})

    def test_mkSimpleProb_negative_probability(self):
        pairs = [(State("A"), -0.1), (State("B"), 1.1)]
        with self.assertRaises(ValueError) as context:
            self.sdp.mkSimpleProb(pairs)
        self.assertEqual(str(context.exception), "No negative probabilities allowed.")

    def test_mkSimpleProb_probabilities_do_not_sum_to_one(self):
        pairs = [(State("A"), 0.5), (State("B"), 0.3)]
        with self.assertRaises(ValueError) as context:
            self.sdp.mkSimpleProb(pairs)
        self.assertTrue("Probabilities do not sum to 1" in str(context.exception))

    def test_mkSimpleProb_probabilities_sum_close_to_one(self):
        pairs = [(State("A"), 0.50000001), (State("B"), 0.49999999)]
        result = self.sdp.mkSimpleProb(pairs)
        self.assertEqual(result, {State("A"): 0.50000001, State("B"): 0.49999999})


if __name__ == "__main__":
    unittest.main()