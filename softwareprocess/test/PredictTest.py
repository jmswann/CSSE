# Tests for dispatch function
# Jeromy Swann

import unittest
import softwareprocess.dispatch as dispatch

class PredictTest(unittest.TestCase):
    def setUp(self):
        pass

    def tearDown(self):
        pass
# --------------------------------------------------
# ---- Acceptance Tests
# 200 predict
    def test100_010ShouldCalculateBetelgeuseCaseAllDefault(self):
        dict = {}
        dict['op'] = 'predict'
        dict['body'] = 'Betelgeuse'
        self.assertEquals(dispatch.dispatch(dict)['long'], '11d41.7')
        self.assertEquals(dispatch.dispatch(dict)['lat'], '7d24.3')

    def test100_020
