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

"""
DEEP MODEL TYPES
"""

"""
Selenium
"""

SELENESE = "SeleneseCommand"

"""
HTTP-related
"""

URL = "URL"
HTTPREQ = "HttpRequest"
ABSHTTPREQ = "AbsHttpRequest"
HTTPRESP = "HttpResponse"
COOKIE = "Cookie"
SETCOOKIE = "SetCookie"

"""
Message encoding related
"""
MULTIPART = "Multipart"
FORMURLENC = "FormURLEncoded"
JSON = "JSON"
_BODY = [MULTIPART, FORMURLENC, JSON]

"""
Server-side Program related
"""
XDEBUG = "Xdebug"
PHPSESSION = "PHPSession"

"""
Disk I/O related
"""
SQL = "SQLQuery"
ABSQUERY = "AbsQuery"
                    

"""
PROPAGATION CHAIN TYPES
"""

UG = "UG"

"""
STATE-CHANGING OPERATION PATTERNS
"""
EVT_UNIQUE_OP = "EVT_UOP"
EVT_REPEATED_OP = "EVT_ROP"

TRACE_SINGLETON_OP = "TR_SINGLETON"
TRACE_REPEATED_OP = "TR_REPEATED"

ABSEVT_OP = "ABSEVT_OP"
