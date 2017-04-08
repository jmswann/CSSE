# dispatch function
# Jeromy Swann

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
        if 'altitude' in values:
            values['error'] = 'altitude already in dictionary'
            return values
        if (not('observation' in values)):
            values['error'] = 'no observation is specified'
            return values
        observation = values['observation']
        if 'd' not in observation:
            values['error'] = 'bad observation'
            return values

        #default values
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
        if x < 0 or x >= 90 or y < 0 or y >= 60.0:
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
        return values
    elif(values['op'] == 'predict'):
        ariesGHADeg = 100
        ariesGHAMin = 42.6
        #starSHADeg = 270
        #starSHAMin = 59.1
        #starDeclination = '7d24.3'
        date = '2001-01-01'
        time = '00:00:00'
        daysPerMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        stars = {'alpheratz': ['357d41.7', '29d10.9'], 'ankaa': ['353d14.1', '-42d13.4'], 'schedar': ['349d38.4', '56d37.7'],
                 'diphda': ['348d54.1', '-17d54.1'], 'achernar': ['335d25.5', '-57d09.7'], 'hamal': ['327d58.7', '23d32.3'],
                 'polaris': ['316d41.3', '89d20.1'], 'akamar': ['315d16.8', '-40d14.8'], 'menkar': ['314d13.0', '4d09.0'],
                 'mirfak': ['308d37.4', '49d55.1'], 'aldebaran': ['290d47.1', '16d32.3'], 'rigel': ['281d10.1', '-8d11.3'],
                 'capella': ['280d31.4', '46d00.7'], 'bellatrix': ['278d29.8', '6d21.6'], 'elnath': ['278d10.1', '28d37.1'],
                 'alnilam': ['275d44.3', '-1d11.8'], 'betelgeuse': ['270d59.1', '7d24.3'], 'canopus': ['263d54.8', '-52d42.5'],
                 'sirius': ['258d31.7', '-16d44.3'], 'adara': ['255d10.8', '-28d59.9'], 'procyon': ['244d57.5', '5d10.9'],
                 'pollux': ['243d25.2', '27d59.0'], 'avior': ['234d16.6', '-59d33.7'], 'suhail': ['222d50.7', '-43d29.8'],
                 'miaplacidus': ['221d38.4', '-69d46.9'], 'alphard': ['217d54.1', '-8d43.8'], 'regulus': ['207d41.4', '11d53.2'],
                 'dubhe': ['193d49.4', '61d39.5'], 'denebola': ['182d31.8', '14d28.9'], 'gienah': ['175d50.4', '-17d37.7'],
                 'acrux': ['173d07.2', '-63d10.9'], 'gacrux': ['171d58.8', '-57d11.9'], 'alioth': ['166d19.4', '55d52.1'],
                 'spica': ['158d29.5', '-11d14.5'], 'alcaid': ['152d57.8', '49d13.8'], 'hadar': ['148d45.5', '-60d26.6'],
                 'menkent': ['148d05.6', '-36d26.6'], 'arcturus': ['145d54.2', '19d06.2'], 'rigil kent.': ['139d49.6', '-60d53.6'],
                 'zubenelg.': ['137d03.7', '-16d06.3'], 'kochab': ['137d21.0', '74d05.2'], 'alphecca': ['126d09.9', '26d39.7'],
                 'antares': ['112d24.4', '-26d27.8'], 'atria': ['107d25.2', '-69d03.0'], 'sabik': ['102d10.9', '-15d44.4'],
                 'shaula': ['96d20.0', '-37d06.6'], 'rasalhague': ['96d05.2', '12d33.1'], 'etamin': ['90d45.9', '51d29.3'],
                 'kaus aust.': ['83d41.9', '-34d22.4'], 'vega': ['80d38.2', '38d48.1'], 'nunki': ['75d56.6', '-26d16.4'],
                 'altair': ['62d06.9', '8d54.8'], 'peacock': ['53d17.2', '-56d41.0'], 'deneb': ['49d30.7', '45d20.5'],
                 'enif': ['33d45.7', '9d57.0'], 'alnair': ['27d42.0', '-46d53.1'], 'fomalhaut': ['15d22.4', '-29d32.3'],
                 'scheat': ['13d51.8', '28d10.3'], 'markab': ['13d36.7', '15d17.6']}
        starSHA = stars[values['body'].lower()][0]
        starSHADeg = int(starSHA.split('d')[0])
        starSHAMin = float(starSHA.split('d')[1])
        starDeclination = stars[values['body'].lower()][1]

        if ('date' in values):
            date = values['date']
        if ('time' in values):
            time = values['time']

        year = int(date.split('-')[0])
        month = int(date.split('-')[1])
        day = int(date.split('-')[2])
        hours = int(time.split(':')[0])
        minutes = int(time.split(':')[1])
        seconds = int(time.split(':')[2])

        daysPassed = 0
        for i in range(1, month):
            daysPassed += daysPerMonth[i]
        daysPassed += (day - 1)
        seconds += daysPassed * 24 * 60 * 60
        seconds += hours * 60 * 60
        seconds += minutes * 60

        cumulativeProgression = (year - 2001) * -14.31667
        leapYears = 0
        for i in range(2001, year, 1):
            if (i % 4 == 0):
                leapYears += 1
        leapProgression = leapYears * abs(360.0 - (86164.1 / 86400 * 360.0)) * 60.0
        rotationInYear = (seconds / 86164.1) * 360.0 * 60.0
        ariesGHAMin += cumulativeProgression + leapProgression + rotationInYear
        ariesGHADeg += math.floor(ariesGHAMin / 60.0)
        ariesGHAMin = ariesGHAMin % 60

        starGHADeg = ariesGHADeg + starSHADeg
        starGHAMin = round(ariesGHAMin, 1) + starSHAMin
        starGHADeg += int(starGHAMin / 60.0)
        starGHADeg = starGHADeg % 360
        starGHAMin = starGHAMin % 60
        values['long'] = str(int(starGHADeg)) + 'd' + str(round(starGHAMin, 1))
        values['lat'] = starDeclination
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
