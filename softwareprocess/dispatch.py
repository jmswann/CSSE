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
        if (not('observation' in values)):
            values['error'] = 'no observation is specified'
            return values
        observation = values['observation']
        if 'd' not in observation:
            values['error'] = 'bad observation'
            return values

        height = 0
        temperature = 72
        pressure = 1010
        horizon = 'natural'

        if ('height' in values):
            try:
                height = float(values['height'])
            except ValueError:
                values['error'] = 'non-numeric height'
                return values
        if ('temperature' in values):
            try:
                temperature = int(values['temperature'])
            except ValueError:
                values['error'] = 'non-integer temperature'
                return values
        if ('pressure' in values):
            try:
                pressure = int(values['pressure'])
            except ValueError:
                values['error'] = 'non-integer pressure'
                return values
        if ('horizon' in values):
            horizon = values['horizon'].lower()
            if horizon != 'natural' and horizon != 'artificial':
                values['error'] = 'invalid horizon type'
                return values

        if height < 0:
            values['error'] = 'height out of bounds'
            return values
        if temperature < -20 or temperature > 120:
            values['error'] = 'temperature out of bounds'
            return values
        if pressure < 100 or pressure > 1100:
            values['error'] = 'pressure out of bounds'
            return values

        x = 0
        y = 0.0
        try:
            x = int(observation.split('d')[0])
            y = float(observation.split('d')[1])
        except ValueError:
            values['error'] = 'bad observation'
            return values
        if x < 0 or x >= 90.0 or y < 0 or y >= 60.0:
            values['error'] = 'observation angle out of bounds'
            return values

        observedAngle = x + (y / 60.0)
        observedAngleInRadians = observedAngle * math.pi / 180.0
        dip = 0
        if (horizon == 'natural'):
            dip = (-0.97 * math.sqrt(height)) / 60
        refraction = (-0.00452 * pressure) / (273 + convertFToC(temperature)) / math.tan(observedAngleInRadians)
        adjustedAltitude = observedAngle + dip + refraction
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
