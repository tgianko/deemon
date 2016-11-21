import types
import url_regex
import re

STRING_TYPE = 0
INT_TYPE = 1
FLOAT_TYPE = 2
BOOL_TYPE = 3
HEX_TYPE = 4
UUID_TYPE = 5
URL_TYPE = 6


def infer_syntactic_type(values):
    instancesOfTypesFound = [0] * 7

    for value in values:
        assert type(
            value) == types.StringType, "All values have to be of type 'String'!"

        guessedType = _infer_basic_type(value)

        if guessedType == STRING_TYPE:
            guessedType = _infer_advanced_type(value)

        instancesOfTypesFound[guessedType] += 1

    # Check whether all values share the same type
    for idx, counter in enumerate(instancesOfTypesFound):
        if counter == len(values):
            return idx

    # If not, check for special case FLOAT_TYPE > INT_TYPE, else it has to be
    # a STRING_TYPE
    if instancesOfTypesFound[INT_TYPE] + instancesOfTypesFound[FLOAT_TYPE] == len(values):
        return FLOAT_TYPE
    elif instancesOfTypesFound[INT_TYPE] + instancesOfTypesFound[HEX_TYPE] == len(values):
        return HEX_TYPE

    return STRING_TYPE


def _infer_basic_type(value):
    dea = BasicTypeDEA()

    for c in value:
        dea.digest(c)

    return dea.get_state()


def _infer_advanced_type(value):
    lowered = value.lower()
    # Bool type
    if lowered in ["true", "false"]:
        return BOOL_TYPE

    # HEX Type
    if re.match(r"^([\d|[a-f])+$", lowered):
        return HEX_TYPE

    # UUID Type
    if re.match(r"^(\d|[a-f]){8}-(\d|[a-f]){4}-(\d|[a-f]){4}-(\d|[a-f]){4}-(\d|[a-f]){12}$", lowered):
        return UUID_TYPE

    # URL type
    if re.match(url_regex.URL_REGEX, value):
        return URL_TYPE

    return STRING_TYPE


class BasicTypeDEA:
    START_STATE = 3
    STRING_STATE = 0
    INT_STATE = 1
    FLOAT_STATE = 2

    DIGIT = 0
    DOT = 1
    ANYTHING_ELSE = 2

    DEA = [[STRING_STATE, STRING_STATE, STRING_STATE], [INT_STATE, FLOAT_STATE, STRING_STATE], [
        FLOAT_STATE, STRING_STATE, STRING_STATE], [INT_STATE, FLOAT_STATE, STRING_STATE]]

    def __init__(self):
        self._current_state = 3

    def digest(self, c):
        ascii_pos = ord(c)
        transition = self.ANYTHING_ELSE  # default case, gets overriden otherwise

        if ascii_pos >= 48 and ascii_pos <= 57:
            transition = self.DIGIT
        elif c == ".":
            transition = self.DOT

        self._current_state = self.DEA[self._current_state][transition]

    def get_state(self):
        if self._current_state == self.START_STATE:
            return self.STRING_STATE

        return self._current_state
