# This file is part of Deemon.

# Deemon is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# Deemon is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with Deemon.  If not, see <http://www.gnu.org/licenses/>.
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
