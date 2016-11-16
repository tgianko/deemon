STRING_TYPE = 0 
INT_TYPE = 1
FLOAT_TYPE = 2
BOOL_TYPE = 3
HEX_TYPE = 4
UUID_TYPE = 5
URL_TYPE = 6

def infer_syntactic_type(values):
    instancesOfTypesFound = []

    for value in values:
        guessedType = _infer_basic_type()

        if basicType == STRING_TYPE:
            guessedType = _infer_advanced_type(guessedType)

        instancesOfTypesFound[guessedType]++

    for idx, counter in enumerate(instancesOfTypesFound):
        if counter == len(values):
            return idx

    if len(instancesOfTypesFound[1]) + len(instancesOfTypesFound[2]) == len(values):
        return "Float"
    else
        return "String"


def _infer_type(value):

def _infer_basic_type(value):
    dea = BasicTypeDEA()
    
    for c in value:
        dea.digest(c)

    return dea.get_state()

def _infer_advanced_type(value):


class BasicTypeDEA:
    STRING_STATE = 0 # this is the start state too
    INT_STATE = 1
    FLOAT_STATE = 2

    DIGIT = 0
    DOT = 1
    ANYTHING_ELSE = 2

    DEA = [[INT_STATE, FLOAT_STATE, STRING_STATE], [INT_STATE, FLOAT_STATE, STRING_STATE], [FLOAT_STATE, STRING_STATE, STRING_STATE]]

    def __init__(self):
        self._current_state = 0

    def digest(c):
        ascii_pos = ord(c)
        transittion

        if ascii_pos >= 48 and ascii_pos<= 57:
            transition = DIGIT
        else if c == ".":
            transition = DOT
        else:
            transition = ANYTHING_ELSE
        
        self._current_state = DEA[self._current_state][transition]

    def get_state():
        return self._current_state
    
