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
