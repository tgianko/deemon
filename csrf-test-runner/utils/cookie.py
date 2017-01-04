from Cookie import BaseCookie, Morsel, _unquote, _quote, _LegalChars

class BetterCookie(BaseCookie):
    # A container class for a set of Morsels
    #

    def value_decode(self, val):
        return _unquote( val ), val

    def value_encode(self, val):
        strval = str(val)
        return strval, _quote( strval )

    def __set(self, key, real_value, coded_value):
        """Private method for setting a cookie's value"""
        M = self.get(key, Morsel())
        M.set(key, real_value, coded_value, LegalChars=_LegalChars + "[]")
        dict.__setitem__(self, key, M)
    # end __set

    def __setitem__(self, key, value):
        """Dictionary style assignment."""
        if isinstance(value, Morsel):
            # allow assignment of constructed Morsels (e.g. for pickling)
            dict.__setitem__(self, key, value)
        else:
            rval, cval = self.value_encode(value)
            self.__set(key, rval, cval)
    # end __setitem__