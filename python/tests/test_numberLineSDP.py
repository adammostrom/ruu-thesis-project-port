

"""
class NextTest(unittest.TestCase):
    
    def assertDictAlmostEqual(self, predicted, expected):
        self.assertEqual(set(predicted.keys()), set(expected.keys()))
        for key in expected:
            self.assertAlmostEqual(predicted[key], expected[key])

    # Checking that sum of probabilities is always 1.
    def test_sum(self):
        self.assertAlmostEqual(sum(nextFuncSimple(0, "0", "Left").values()), 1)
        self.assertAlmostEqual(sum(nextFuncSimple(0, "0", "Stay").values()), 1)
        self.assertAlmostEqual(sum(nextFuncSimple(0, "2", "Left").values()), 1)
        self.assertAlmostEqual(sum(nextFuncSimple(0, "2", "Stay").values()), 1)

 def test_trivial(self):
        self.assertDictAlmostEqual(nextFuncSimple(0, "0", "Left"), {'-1': 0.85, '0': 0.1, '1': 0.05})
        self.assertDictAlmostEqual(nextFuncSimple(0, "0", "Stay"), {'-1': 0.1, '0': 0.8, '1': 0.1})
        self.assertDictAlmostEqual(nextFuncSimple(0, "0", "Right"), {'-1': 0.05, '0': 0.1, '1': 0.85})

        self.assertDictAlmostEqual(nextFuncSimple(1, "2", "Left"), {'1': 0.85, '2': 0.15})
        self.assertDictAlmostEqual(nextFuncSimple(1, "2", "Stay"), {'1': 0.1, '2': 0.9})
        self.assertDictAlmostEqual(nextFuncSimple(1, "2", "Right"), {'1': 0.05, '2': 0.95})


"""