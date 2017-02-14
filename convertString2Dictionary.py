# Created on: February 13, 2017
# @author: Jeromy Swann
import urllib

def convertString2Dictionary(inputString = ""):
    errorDict = {'error':'true'}
    letters = {'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u',
               'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
               'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'}
    numbers = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'}
    symbols = {'=', '.', ',', ' '}

    decodedString = urllib.unquote(inputString).strip()
    if (decodedString == ''): #make sure decoded string not empty
        return errorDict
    for k in range(0, len(decodedString)): #make sure decoded strings contains only valid characters
        if (decodedString[k] not in letters and decodedString[k] not in numbers and decodedString[k] not in symbols):
            return errorDict

    pairs = decodedString.split(',')
    dict = {}
    for pair in pairs:
        pair = pair.strip()
        if (pair == ''): #make sure key-value pair is not empty
            return errorDict

        pairSplit = pair.split('=')

        if (len(pairSplit) != 2): #make sure there are two elements
            return errorDict

        key = pairSplit[0].strip()
        if (key[0] in numbers): #make sure key doesn't start with a number
            return errorDict
        for i in range(1, len(key)): #make sure key is made of letters, numbers, and period
            if (key[i] not in numbers and key[i] not in letters and key[i] != '.'):
                return errorDict

        value = pairSplit[1].strip()
        for j in range(0, len(value)): #make sure value is made up of letters, numbers and period
            if (value[j] not in numbers and value[j] not in letters and value[j] != '.'):
                return errorDict
        if (key == '' or value == ''): #make sure neither key nor value is empty
            return errorDict
        if key in dict: #make sure not a duplicate key
            return errorDict

        dict[key] = value

    return dict
