import math

def dispatch(values=None):

    #Validate parm
    if(values == None):
        return {'error': 'parameter is missing'}
    if(not(isinstance(values,dict))):
        return {'error': 'parameter is not a dictionary'}
    if (not('op' in values)):
        values['error'] = 'no op  is specified'
        return values

    #Perform designated function
    if(values['op'] == 'adjust'):
        observation = values['observation']
        height = 0
        temperature = 72
        pressure = 1010
        horizon = 'natural'

        if ('height' in values):
            height = values['height']

        x = int(observation.split('d')[0])
        #print x
        y = float(observation.split('d')[1])
        #print y
        observedAngle = x + (y / 60.0)
        observedAngleInRadians = observedAngle * math.pi / 180.0
        dip = (-0.97 * math.sqrt(height)) / 60
        refraction = (-0.00452 * pressure) / (273 + convertFToC(temperature)) / math.tan(observedAngleInRadians)
        #print (-0.00452 * pressure)
        #print (273 + convertFToC(temperature))
        #print math.tan(observedAngleInRadians)
        #print refraction
        adjustedAltitude = observedAngle + dip + refraction
        #print adjustedAltitude
        adjustedAltitudeX = int(adjustedAltitude)
        adjustedAltitudeY = round((adjustedAltitude - adjustedAltitudeX) * 60.0, 1)
        adjustedAltitudeString = str(adjustedAltitudeX) + 'd' + str(adjustedAltitudeY)
        values['altitude'] = adjustedAltitudeString
        return values    #<-------------- replace this with your implementation
    elif(values['op'] == 'predict'):
        return values    #This calculation is stubbed out
    elif(values['op'] == 'correct'):
        return values    #This calculation is stubbed out
    elif(values['op'] == 'locate'):
        return values    #This calculation is stubbed out
    else:
        values['error'] = 'op is not a legal operation'
        return values

def convertFToC(temperatureInF):
    return (temperatureInF - 32) * 5.0 / 9.0
