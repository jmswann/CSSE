import unittest
import softwareprocess.dispatch as dispatch

class DispatchTest(unittest.TestCase):
    def setUp(self):
        pass

    def tearDown(self):
        pass
# --------------------------------------------------
# ---- Acceptance Tests
# 100 dispatch
#   Desired level of confidence: Equivalence partition analysis
#   Input-output Analysis
#       inputs:     values ->   dictionary with key-value pairs as follows:
#                   observation ->  string of form "xdy.y"  mandatory, unvalidated
#                                   where x ->  integer .GE. 0 and .LT. 90
#                                   and y ->    float .GE. 0.0 and .LT. 60.0
#                   height ->   string of numeric value .GE. 0  optional, unvalidated
#                   temperature ->  string of integer value .GE. -20 and .LE. 120   optional, unvalidated
#                   pressure -> string of integer value .GE. 100 and .LE. 1100  optional, unvalidated
#                   horizon ->  string of value "natural" or "artificial"   optional, unvalidated
#       outputs:    values ->   dictionary with added key 'altitude' or 'error' and associated value
#       Happy Path Analysis:
#           observation:    nominal value observation="42d0.0"
#                           2nd nominal value observation="45d30.0"
#           height:     nominal value height="10"
#                       missing height
#           temperature:    nominal value temperature="100"
#                           missing temperature
#           pressure:   nominal value pressure="500"
#                       missing pressure
#           horizon:    non-default value horizon="artificial"
#                       missing horizon
#           output:
#               The output is an interaction of observation, height, temperature, pressure, and horizon
#                   nominal observation, missing height, missing temperature, missing pressure, missing horizon
#                   2nd nominal observation, nominal height, missing temperature, missing pressure, missing horizon
#                   2nd nominal observation, nominal height, nominal temperature, missing pressure, missing horizon
#                   2nd nominal observation, nominal height, nominal temperature, nominal pressure, missing horizon
#                   2nd nominal observation, nominal height, nominal temperature, nominal pressure, non-default horizon
#       Sad Path Analysis:
#
#
# Happy Path
    def test100_010ShouldCalculateNominalCaseAllDefault(self):
        dict = {}
        dict['op'] = 'adjust'
        dict['observation'] = '42d0.0'
        self.assertEquals(dispatch.dispatch(dict)['altitude'], '41d59.0')

    def test100_020ShouldCalculateNominalCaseNominalHeightRestDefault(self):
        dict = {}
        dict['op'] = 'adjust'
        dict['observation'] = '45d30.0'
        dict['height'] = '10'
        self.assertEquals(dispatch.dispatch(dict)['altitude'], '45d26.0')

    def test100_030ShouldCalculateNominalCaseNominalHeightNominalTempRestDefault(self):
        dict = {}
        dict['op'] = 'adjust'
        dict['observation'] = '45d30.0'
        dict['height'] = '10'
        dict['temperature'] = '100'
        self.assertEquals(dispatch.dispatch(dict)['altitude'], '45d26.1')

    def test100_040ShouldCalculateNominalCaseNominalHeightNominalTempNominalPressureDefaultHorizon(self):
        dict = {}
        dict['op'] = 'adjust'
        dict['observation'] = '45d30.0'
        dict['height'] = '10'
        dict['temperature'] = '100'
        dict['pressure'] = '500'
        self.assertEquals(dispatch.dispatch(dict)['altitude'], '45d26.5')

    def test100_050ShouldCalculateNominalCaseAllValidNonDefault(self):
        dict = {}
        dict['op'] = 'adjust'
        dict['observation'] = '45d30.0'
        dict['height'] = '10'
        dict['temperature'] = '100'
        dict['pressure'] = '500'
        dict['horizon'] = 'artificial'
        self.assertEquals(dispatch.dispatch(dict)['altitude'], '45d29.6')

    def test100_060ShouldGiveErrorForObservationDegreesBelowBounds(self):
        dict = {}
        dict['op'] = 'adjust'
        dict['observation'] = '-1d30.0'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'Observation angle out of bounds')
