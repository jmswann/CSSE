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
#           observation:    degrees below bounds
#                           degrees above bounds
#                           minutes below bounds
#                           minutes above bounds
#                           missing observation
#                           missing 'd'
#                           non-integer degrees
#                           non-numeric minutes
#           height:     height below bounds
#                       non-numeric height
#           temperature:    temperature below bounds
#                           temperature above bounds
#                           non-integer temperature
#           pressure:   pressure below bounds
#                       pressure above bounds
#                       non-integer pressure
#           horizon:    invalid horizon type
#           altitude:   altitude already present in dictionary
#
# Happy Path
    def test100_010ShouldCalculateNominalCaseAllDefault(self):
        dict = {}
        dict['op'] = 'predict'
        dict['body'] = 'Betelgeuse'
        self.assert(dispatch.dispatch(dict)['long'], '11d41.7')
        self.assert(dispatch.dispatch(dict)['lat'], '7d24.3')
