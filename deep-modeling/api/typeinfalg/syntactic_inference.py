import types
import url_regex
import re

SYN_TYPE_STRING = 0
SYN_TYPE_INT = 1
SYN_TYPE_FLOAT = 2
SYN_TYPE_BOOL = 3
SYN_TYPE_HEX = 4
SYN_TYPE_UUID = 5
SYN_TYPE_URL = 6
SYN_TYPE_PATH = 7


def infer_syntactic_type(values):
    instancesOfTypesFound = [0] * 8

    for value in values:
        assert type(
            value) == types.StringType, "All values have to be of type 'String'!"

        guessedType = _infer_basic_type(value)

        if guessedType == SYN_TYPE_STRING:
            guessedType = _infer_advanced_type(value)

        instancesOfTypesFound[guessedType] += 1

    # Check whether all values share the same type
    for idx, counter in enumerate(instancesOfTypesFound):
        if counter == len(values):
            return idx

    # If not, check for special case SYN_TYPE_FLOAT > SYN_TYPE_INT, else it has to be
    # a STRING_TYPE
    if instancesOfTypesFound[SYN_TYPE_INT] + instancesOfTypesFound[SYN_TYPE_FLOAT] == len(values):
        return SYN_TYPE_FLOAT
    elif instancesOfTypesFound[SYN_TYPE_INT] + instancesOfTypesFound[SYN_TYPE_HEX] == len(values):
        return SYN_TYPE_HEX

    return SYN_TYPE_STRING


def _infer_basic_type(value):
    dea = BasicTypeDEA()

    for c in value:
        dea.digest(c)

    return dea.get_state()


def _infer_advanced_type(value):
    lowered = value.lower()
    # Bool type
    if lowered in ["true", "false"]:
        return SYN_TYPE_BOOL

    # HEX Type
    if re.match(r"^([\d|[a-f])+$", lowered):
        return SYN_TYPE_HEX

    # UUID Type
    if re.match(r"^(\d|[a-f]){8}-(\d|[a-f]){4}-(\d|[a-f]){4}-(\d|[a-f]){4}-(\d|[a-f]){12}$", lowered):
        return SYN_TYPE_UUID

    # URL type
    if re.match(url_regex.URL_REGEX, lowered):
        return SYN_TYPE_URL

    if re.match(r"^((\S+/\S*)|(\S*/\S+))$", lowered):
        return SYN_TYPE_PATH    

    return SYN_TYPE_STRING


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
