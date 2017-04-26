# Tests for dispatch function
# Jeromy Swann

import unittest
import softwareprocess.dispatch as dispatch

class DispatchTest(unittest.TestCase):
    def setUp(self):
        pass

    def tearDown(self):
        pass
# --------------------------------------------------
# ---- Acceptance Tests
# 100 adjust
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
        self.assertEquals(dispatch.dispatch(dict)['error'], 'observation angle out of bounds')

    def test100_070ShouldGiveErrorForObservationDegreesAboveBounds(self):
        dict = {}
        dict['op'] = 'adjust'
        dict['observation'] = '90d30.0'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'observation angle out of bounds')

    def test100_080ShouldGiveErrorForObservationMinutesBelowBounds(self):
        dict = {}
        dict['op'] = 'adjust'
        dict['observation'] = '45d-1.0'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'observation angle out of bounds')

    def test100_090ShouldGiveErrorForObservationMinutesAboveBounds(self):
        dict = {}
        dict['op'] = 'adjust'
        dict['observation'] = '45d60.0'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'observation angle out of bounds')

    def test100_100ShouldGiveErrorForHeightBelowBounds(self):
        dict = {}
        dict['op'] = 'adjust'
        dict['observation'] = '45d30.0'
        dict['height'] = '-1'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'height out of bounds')

    def test100_110ShouldGiveErrorForTemperatureBelowBounds(self):
        dict = {}
        dict['op'] = 'adjust'
        dict['observation'] = '45d30.0'
        dict['temperature'] = '-21'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'temperature out of bounds')

    def test100_120ShouldGiveErrorForTemperatureAboutBounds(self):
        dict = {}
        dict['op'] = 'adjust'
        dict['observation'] = '45d30.0'
        dict['temperature'] = '121'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'temperature out of bounds')

    def test100_130ShouldGiveErrorForPressureBelowBounds(self):
        dict = {}
        dict['op'] = 'adjust'
        dict['observation'] = '45d30.0'
        dict['pressure'] = '99'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'pressure out of bounds')

    def test100_140ShouldGiveErrorForPressureAboveBounds(self):
        dict = {}
        dict['op'] = 'adjust'
        dict['observation'] = '45d30.0'
        dict['pressure'] = '1101'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'pressure out of bounds')

    def test100_150ShouldGiveErrorForMissingObservation(self):
        dict = {}
        dict['op'] = 'adjust'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'no observation is specified')

    def test100_160ShouldGiveErrorForObservationWithoutD(self):
        dict = {}
        dict['op'] = 'adjust'
        dict['observation'] = '4530.0'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'bad observation')

    def test100_170ShouldGiveErrorForNonIntegerDegrees(self):
        dict = {}
        dict['op'] = 'adjust'
        dict['observation'] = 'xd30.0'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'bad observation')

    def test100_180ShouldGiveErrorForNonNumericMinutes(self):
        dict = {}
        dict['op'] = 'adjust'
        dict['observation'] = '45dy.y'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'bad observation')

    def test100_190ShouldGiveErrorForNonNumericHeight(self):
        dict = {}
        dict['op'] = 'adjust'
        dict['observation'] = '45d30.0'
        dict['height'] = 'blah'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'non-numeric height')

    def test100_200ShouldGiveErrorForNonIntegerTemperature(self):
        dict = {}
        dict['op'] = 'adjust'
        dict['observation'] = '45d30.0'
        dict['temperature'] = 'blah'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'non-integer temperature')

    def test100_210ShouldGiveErrorForNonIntegerPressure(self):
        dict = {}
        dict['op'] = 'adjust'
        dict['observation'] = '45d30.0'
        dict['pressure'] = 'blah'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'non-integer pressure')

    def test100_220ShouldGiveErrorForInvalidHorizon(self):
        dict = {}
        dict['op'] = 'adjust'
        dict['observation'] = '45d30.0'
        dict['horizon'] = 'blah'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'invalid horizon type')

    def test100_230ShouldGiveErrorIfAltitudeAlreadyInDictionary(self):
        dict = {}
        dict['op'] = 'adjust'
        dict['observation'] = '45d30.0'
        dict['altitude'] = '45d30.0'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'altitude already in dictionary')

# 200 predict
    def test200_010ShouldCalculateBetelgeuseCaseAllDefault(self):
        dict = {}
        dict['op'] = 'predict'
        dict['body'] = 'Betelgeuse'
        self.assertEquals(dispatch.dispatch(dict)['long'], '11d41.7')
        self.assertEquals(dispatch.dispatch(dict)['lat'], '7d24.3')

    def test200_020ShouldCalculateBetelgeuseCase20160101DefaultTime(self):
        dict = {}
        dict['op'] = 'predict'
        dict['body'] = 'Betelgeuse'
        dict['date'] = '2016-01-01'
        self.assertEquals(dispatch.dispatch(dict)['long'], '11d3.9')
        self.assertEquals(dispatch.dispatch(dict)['lat'], '7d24.3')

    def test200_030ShouldCalculateBetelgeuseCase2016117At031542(self):
        dict = {}
        dict['op'] = 'predict'
        dict['body'] = 'Betelgeuse'
        dict['date'] = '2016-01-17'
        dict['time'] = '03:15:42'
        self.assertEquals(dispatch.dispatch(dict)['long'], '75d53.6')
        self.assertEquals(dispatch.dispatch(dict)['lat'], '7d24.3')

    def test_200_040ShouldCalculateAlpheratzCase20170110At084320(self):
        dict = {}
        dict['op'] = 'predict'
        dict['body'] = 'Alpheratz'
        dict['date'] = '2017-01-10'
        dict['time'] = '08:43:20'
        self.assertEquals(dispatch.dispatch(dict)['long'], '238d34.9')
        self.assertEquals(dispatch.dispatch(dict)['lat'], '29d10.9')

    def test_200_050ShouldGiveErrorOnMissingBody(self):
        dict = {}
        dict['op'] = 'predict'
        dict['date'] = '2017-01-10'
        dict['time'] = '08:43:20'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'mandatory information is missing')

    def test_200_060ShouldGiveErrorOnInvalidBody(self):
        dict = {}
        dict['op'] = 'predict'
        dict['body'] = 'pineapple'
        dict['date'] = '2017-01-10'
        dict['time'] = '08:43:20'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'star not in catalog')

    def test_200_070ShouldGiveErrorOnInvalidDate(self):
        dict = {}
        dict['op'] = 'predict'
        dict['body'] = 'Alpheratz'
        dict['date'] = '1995-01-10'
        dict['time'] = '08:43:20'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'invalid date')

    def test_200_080ShouldGiveErrorOnInvalidTime(self):
        dict = {}
        dict['op'] = 'predict'
        dict['body'] = 'Alpheratz'
        dict['date'] = '2017-01-10'
        dict['time'] = '35:43:20'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'invalid time')

    def test_200_090ShouldGiveErrorIfDictionaryContainsLat(self):
        dict = {}
        dict['op'] = 'predict'
        dict['body'] = 'Alpheratz'
        dict['date'] = '2017-01-10'
        dict['time'] = '08:43:20'
        dict['lat'] = '34d18.6'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'lat already in dictionary')

    def test_200_100ShouldGiveErrorIfDictionaryContainsLong(self):
        dict = {}
        dict['op'] = 'predict'
        dict['body'] = 'Alpheratz'
        dict['date'] = '2017-01-10'
        dict['time'] = '08:43:20'
        dict['long'] = '34d18.6'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'long already in dictionary')

# 200 correct
    def test300_010ShouldCalculateNominalCase1(self):
        dict = {}
        dict['op'] = 'correct'
        dict['lat'] = '89d20.1'
        dict['long'] = '154d5.4'
        dict['altitude'] = '37d17.4'
        dict['assumedLat'] = '35d59.7'
        dict['assumedLong'] = '74d35.3'
        self.assertEquals(dispatch.dispatch(dict)['correctedDistance'], '104')
        self.assertEquals(dispatch.dispatch(dict)['correctedAzimuth'], '0d36.8')

    def test300_020ShouldCalculateNominalCase2(self):
        dict = {}
        dict['op'] = 'correct'
        dict['lat'] = '16d32.3'
        dict['long'] = '95d41.6'
        dict['altitude'] = '13d42.3'
        dict['assumedLat'] = '-53d38.4'
        dict['assumedLong'] = '74d35.3'
        self.assertEquals(dispatch.dispatch(dict)['correctedDistance'], '3950')
        self.assertEquals(dispatch.dispatch(dict)['correctedAzimuth'], '164d42.9')

    def test300_030ShouldGiveErrorOnMissingElement(self):
        dict = {}
        dict['op'] = 'correct'
        dict['long'] = '95d41.6'
        dict['altitude'] = '13d42.3'
        dict['assumedLat'] = '-53d38.4'
        dict['assumedLong'] = '74d35.3'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'mandatory information is missing')

    def test300_040ShouldGiveErrorOnOutOfBoundsLat1(self):
        dict = {}
        dict['op'] = 'correct'
        dict['lat'] = '100d20.0'
        dict['long'] = '95d41.6'
        dict['altitude'] = '13d42.3'
        dict['assumedLat'] = '-53d38.4'
        dict['assumedLong'] = '74d35.3'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'invalid lat')

    def test300_050ShouldGiveErrorOnOutOfBoundsLat2(self):
        dict = {}
        dict['op'] = 'correct'
        dict['lat'] = '30d90.0'
        dict['long'] = '95d41.6'
        dict['altitude'] = '13d42.3'
        dict['assumedLat'] = '-53d38.4'
        dict['assumedLong'] = '74d35.3'
        self.assertEquals(dispatch.dispatch(dict)['error'], 'invalid lat')

# UNIT TESTS
    def test400_010CalculateLHANominal(self):
        self.assertEquals(dispatch.calculateLocalHourAngle('95d41.6', '74d35.3'), '170d16.9')

    def test500_010CalculateIntermediateDistanceNominal(self):
        self.assertAlmostEquals(dispatch.calculateIntermediateDistance('16d32.3', '-53d38.4', '170d16.9'), -0.789, 3)

    def test600_010CalculateCorrectedAltitudeNominal(self):
        self.assertEquals(dispatch.calculateCorrectedAltitude(-0.789410565017742), '-52d7.8')

    def test700_010CalculateCorrectedDistanceNominal(self):
        self.assertEquals(dispatch.calculateCorrectedDistance('13d42.3', '-52d7.8'), '65d50.1')

    def test800_010CalculateCorrectedAzimuthNominal(self):
        self.assertEquals(dispatch.calculateCorrectedAzimuth('89d20.1', '35d59.7', 0.581474856), '0d36.8')

    def test900_010AddToDictNominal(self):
        dict = {}
        dispatch.addToDict(dict, 'key', 'value')
        self.assertIn('key', dict)
        self.assertEquals(dict['key'], 'value')


