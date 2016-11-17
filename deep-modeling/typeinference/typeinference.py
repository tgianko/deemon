import types

STRING_TYPE = 0 
INT_TYPE = 1
FLOAT_TYPE = 2
BOOL_TYPE = 3
HEX_TYPE = 4
UUID_TYPE = 5
URL_TYPE = 6

def infer_syntactic_type(values):
    instancesOfTypesFound = [0]*7

    for value in values:
        assert type(value) == types.StringType, "All values have to be of type string!"

        guessedType = _infer_basic_type(value)

        if guessedType == STRING_TYPE:
            guessedType = _infer_advanced_type(guessedType)

        instancesOfTypesFound[guessedType] = instancesOfTypesFound[guessedType] + 1 

    for idx, counter in enumerate(instancesOfTypesFound):
        if counter == len(values):
            return idx

    if instancesOfTypesFound[1] + instancesOfTypesFound[2] == len(values):
        return FLOAT_TYPE
    else:
        return STRING_TYPE


def _infer_basic_type(value):
    dea = BasicTypeDEA()
    
    for c in value:
        dea.digest(c)

    return dea.get_state()


def _infer_advanced_type(value):
    return 0


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

    def digest(self, c):
        ascii_pos = ord(c)
        transition = self.ANYTHING_ELSE

        if ascii_pos >= 48 and ascii_pos<= 57:
            transition = self.DIGIT
        elif c == ".":
            transition = self.DOT
        
        self._current_state = self.DEA[self._current_state][transition]

    def get_state(self):
        return self._current_state
    
