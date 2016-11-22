import types
import url_regex
import re
from type_enum import *

SYN_TYPE_STRING = TypeEnum("str")
SYN_TYPE_INT = TypeEnum("int")
SYN_TYPE_FLOAT = TypeEnum("float")
SYN_TYPE_BOOL = TypeEnum("bool")
SYN_TYPE_HEX = TypeEnum("hex")
SYN_TYPE_UUID = TypeEnum("uuid")
SYN_TYPE_URL = TypeEnum("url")
SYN_TYPE_PATH = TypeEnum("path")


def infer_syntactic_type(values):
    instancesOfTypesFound = [0] * TypeEnum.size()

    for value in values:
        value = unicode(value)

        guessedType = TypeEnum.get_by_id(_infer_basic_type(value))

        if guessedType == SYN_TYPE_STRING:
            guessedType = _infer_advanced_type(value)

        instancesOfTypesFound[int(guessedType)] += 1

    # Check whether all values share the same type
    for idx, counter in enumerate(instancesOfTypesFound):
        if counter == len(values):
            return TypeEnum.get_by_id(idx)

    # If not, check for special case SYN_TYPE_FLOAT > SYN_TYPE_INT, else it has to be
    # a STRING_TYPE
    if instancesOfTypesFound[int(SYN_TYPE_INT)] + instancesOfTypesFound[int(SYN_TYPE_FLOAT)] == len(values):
        return SYN_TYPE_FLOAT
    elif instancesOfTypesFound[int(SYN_TYPE_INT)] + instancesOfTypesFound[int(SYN_TYPE_HEX)] == len(values):
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

    # Path type
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
        codepoint = ord(c)
        transition = self.ANYTHING_ELSE  # default case, gets overriden otherwise

        if codepoint >= 48 and codepoint <= 57:
            transition = self.DIGIT
        elif c == ".":
            transition = self.DOT

        self._current_state = self.DEA[self._current_state][transition]

    def get_state(self):
        return_state = self._current_state

        if self._current_state == self.START_STATE:
            return_state = self.STRING_STATE

        return return_state
