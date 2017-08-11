"""
https://gist.github.com/dperini/729294

Regular Expression for URL validation

Author: Diego Perini
Updated: 2010/12/05
License: MIT

Copyright (c) 2010-2013 Diego Perini (http://www.iport.it)

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

the regular expression composed & commented
could be easily tweaked for RFC compliance,
it was expressly modified to fit & satisfy
these test for an URL shortener:

  http://mathiasbynens.be/demo/url-regex

Notes on possible differences from a standard/generic validation:

- utf-8 char class take in consideration the full Unicode range
- TLDs have been made mandatory so single names like "localhost" fails
- protocols have been restricted to ftp, http and https only as requested
"""

import re

URL_REGEX = re.compile(
    u"^"
    # COMMENT: protocol identifier
    u"(?:(?:https?|ftp)://)"
    # COMMENT: user:pass authentication
    u"(?:\S+(?::\S*)?@)?"
    u"(?:"
    # COMMENT: IP address exclusion
    # COMMENT: private & local networks
    u"(?!(?:10|127)(?:\.\d{1,3}){3})"
    u"(?!(?:169\.254|192\.168)(?:\.\d{1,3}){2})"
    u"(?!172\.(?:1[6-9]|2\d|3[0-1])(?:\.\d{1,3}){2})"
    # COMMENT: IP address dotted notation octets
    # COMMENT: excludes loopback network 0.0.0.0
    # COMMENT: excludes reserved space >= 224.0.0.0
    # COMMENT: excludes network & broadcast addresses
    # COMMENT: (first & last IP address of each class)
    u"(?:[1-9]\d?|1\d\d|2[01]\d|22[0-3])"
    u"(?:\.(?:1?\d{1,2}|2[0-4]\d|25[0-5])){2}"
    u"(?:\.(?:[1-9]\d?|1\d\d|2[0-4]\d|25[0-4]))"
    u"|"
    # COMMENT: host name
    u"(?:(?:[a-z\u00a1-\uffff0-9]-?)*[a-z\u00a1-\uffff0-9]+)"
    # COMMENT: domain name
    u"(?:\.(?:[a-z\u00a1-\uffff0-9]-?)*[a-z\u00a1-\uffff0-9]+)*"
    # COMMENT: TLD identifier
    u"(?:\.(?:[a-z\u00a1-\uffff]{2,}))"
    u")"
    # COMMENT: port number
    u"(?::\d{2,5})?"
    # COMMENT: resource path
    u"(?:/\S*)?"
    u"$"
    , re.UNICODE)
