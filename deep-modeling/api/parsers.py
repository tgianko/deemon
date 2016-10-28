from urlparse import urlparse, parse_qs
from datamodel.core import *
from cStringIO import StringIO
import multipart
import re
import sqlparse
import sqlparse.tokens as sqlptokens
from dm_types import *
import Cookie
from string import lstrip
from enum import Enum


def parse_url(url, projname):
    scheme, netloc, path, params, query, fragment = urlparse(url)

    url_n = ParseTree(projname, URL, url)

    pos = 0

    url_n.HasChild.add(PTTerminalNode(projname, URL, scheme, "scheme", pos))
    pos += 1
    url_n.HasChild.add(PTTerminalNode(projname, URL, netloc, "netloc",pos))
    pos += 1

    if len(path) > 0:
        url_n.HasChild.add(PTTerminalNode(projname, URL, path, "path", pos))
        pos += 1

    if len(params) > 0:
        url_n.HasChild.add(PTTerminalNode(projname, URL, params, "params", pos))
        pos += 1
    
    if len(query) > 0:
        # Create KeyValuePair
        query_n = PTNonTerminalNode(projname, URL, "query-string",pos)
        pos += 1
        q_pos = 0  
        for k, vs in parse_qs(query).iteritems():
            for v in vs:
                query_n.HasChild.add(PTTerminalNode(projname, URL, k, "param-name", q_pos))
                query_n.HasChild.add(PTTerminalNode(projname, URL, v, "param-value", q_pos+1))
                q_pos += 2
        url_n.HasChild.add(query_n)


        
    if len(fragment) > 0:
        url_n.HasChild.add(PTTerminalNode(projname, URL, fragment, "fragment", pos))

    return url_n


IGNORE_HEADERS = ["accept-.*", "connection", "date", "expect",
                  "if-.*", "range", "user-agent", "age", "cache-.*",
                  "content-length", "content-language", "content-encoding",
                  "content-range", "etag", "expires", "last-.*", "transfer-.*",
                  "vary", "x-powered-by", "server", "pragma",
                  "upgrade-insecure-requests"]


def headers_to_list(hdrs):
    if hdrs[-2:] == "\r\n":
        hdrs = hdrs[:-2]
    
    hdrs = hdrs.split("\r\n")
    hdrs = [(hdr.split(":", 1)[0], hdr.split(":", 1)[1])
            for hdr in hdrs]  # from list of strings to  list of (k,v)
    return hdrs

def parse_cookie(hdrname, hdrval, projname):
    dm_type = None
    if hdrname.lower() == "cookie":
        dm_type = COOKIE
    elif hdrname.lower() == "set-cookie":
        dm_type = SETCOOKIE
    
    n = ParseTree(projname, dm_type, hdrval)   

    cookiep = Cookie.SimpleCookie()
    cookiep.load(str(hdrval))

    pos = 0

    for k in cookiep:
        kv_n = PTNonTerminalNode(projname, dm_type, "cookie-pair", pos)
        k_n  = PTTerminalNode(projname, dm_type, k, "cookie-name", 0) 
        v_n  = PTTerminalNode(projname, dm_type, cookiep[k].value, "cookie-value", 1)
        kv_n.HasChild.add(k_n)
        kv_n.HasChild.add(v_n)
        n.HasChild.add(kv_n)
        pos+=1
    return n

def parse_headers(hdrs, projname, dm_type):
    hdrs = headers_to_list(hdrs)

    def keep(h):
        for r in IGNORE_HEADERS:
            if re.match(r, h[0]) is not None:
                return False
        return True
    hdrs = filter(keep, hdrs)  # Remove blacklisted headers

    hdrs_n = PTNonTerminalNode(projname, dm_type, "header-list", -1)
    pos = 0
    for k, v in hdrs:
        v = v.strip()
        kv_n = PTNonTerminalNode(projname, dm_type, "header-field", pos)
        k_n  = PTTerminalNode(projname, dm_type, k, "field-name", 0)
        if k.lower() in ["cookie", "set-cookie"]:
            v_n = parse_cookie(k, v, projname)
            v_n.pos = pos
        elif k.lower() in ["referer", "origin"]:
            v_n = parse_url(v, projname)
            v_n.pos = pos
        else:
            v_n  = PTTerminalNode(projname, dm_type, v, "field-value", 1)
        
        kv_n.HasChild.add(k_n)
        kv_n.HasChild.add(v_n)
        pos+=1
        hdrs_n.HasChild.add(kv_n)
    
    return hdrs_n


def content_type(hdrs):
    hdrs = headers_to_list(hdrs)
    for k, v in hdrs:
        if k.lower() == "content-type":
            return v
    return None


def parse_body(body, ctype, projname, dm_type):

    if "multipart/form-data" in ctype:
        # parse multipart/form-data body
        body_n = ParseTree(projname, MULTIPART, "multipart body")
        s_obj = StringIO(body)
        boundary = ctype.split("; boundary=")[1]
        mp = multipart.MultipartParser(s_obj, boundary)
        pos = 0
        for part in mp:
            k = part.options.get("name")
            v = part.value
            kv_n = PTNonTerminalNode(projname, MULTIPART, "multipart-pair", pos)
            k_n  = PTTerminalNode(projname, dm_type, k, "multipart-name", 0)
            v_n  = PTTerminalNode(projname, dm_type, v, "multipart-value", 1)
            kv_n.HasChild.add(k_n)
            kv_n.HasChild.add(v_n)
            body_n.HasChild.add(kv_n)
            pos+=1

        return body_n
    elif "application/x-www-form-urlencoded" in ctype:
        raise Exception("application/x-www-form-urlencoded")
        # parse application/x-www-form-urlencoded body
        # for k, vs in parse_qs(query).iteritems():  # TODO:query is undefined
        #    for v in vs:
        #        p = KeyValuePair(projname, k, v)
        #        body_n.Contains.add(p)
    else:
        # No JSON yet...
        body_n = ParseTree(projname, ctype, body[0:128])
        s_n = PTTerminalNode(projname, dm_type, body, "plaintext-body", 0)
        body_n.HasChild.add(s_n)
        return body_n
    
    


def parse_httpreq(method, url, hdrs, body, seq, ts, projname, session, user):
    hreq_n = ParseTree(projname, HTTPREQ, "{} {}".format(method, url))

    hreq_n.HasChild.add(PTTerminalNode(projname, HTTPREQ, method, "method",  0))

    url_n = parse_url(url, projname)
    url_n.pos = 1
    hreq_n.HasChild.add(url_n)

    hdrs_n = parse_headers(hdrs, projname, HTTPREQ)
    hdrs_n.pos = 2
    hreq_n.HasChild.add(hdrs_n)

    ctype = content_type(hdrs)
    if ctype:
        body_n = parse_body(body, ctype, projname, HTTPREQ)
        body_n.pos = 3
        hreq_n.HasChild.add(body_n)

    return hreq_n


def parse_httpres(status, hdrs, body, seq, ts, projname, session, user):
    hres_n = ParseTree(projname, HTTPRESP, status)

    hres_n.HasChild.add(PTTerminalNode(projname, HTTPRESP, status, "HTTP-version", 0))

    hdrs_n = parse_headers(hdrs, projname, HTTPRESP)
    hdrs_n.pos = 1
    hres_n.HasChild.add(hdrs_n)

    ctype = content_type(hdrs)
    if ctype:
        body_n = parse_body(body, ctype, projname, HTTPRESP)
        body_n.pos = 2
        hres_n.HasChild.add(body_n)

    return hres_n



def parse_selcmd(command, target, value, seq, ts, projname, session, user):
    sel_n = ParseTree(projname, SELENESE, "command={} target={} value={}".format(command, target, value))
    
    pairs = [("command", command, 0), ("target", target, 1), ("value", value, 2)]
    for n, v, i in pairs:
        nt = PTNonTerminalNode(projname, SELENESE, "selenese-pair".format(n), i)
        t_n  = PTTerminalNode(projname, SELENESE, n, "name", 0)
        t_v  = PTTerminalNode(projname, SELENESE, v, "value", 1)
        nt.HasChild.add(t_n)
        nt.HasChild.add(t_v)
        sel_n.HasChild.add(nt)

    return sel_n


def visit_sql_pt(pt, i, n):
    for el in pt.tokens:
        if el.is_group():
            child = PTNonTerminalNode(n.projname, SQL, "token-list", i)
            n.HasChild.add(child)
            # print "SQLTokenList"
            visit_sql_pt(el, 0, child)
        else:
            if el.ttype == sqlptokens.Token.Punctuation or\
               el.ttype == sqlptokens.Token.Text.Whitespace:
                continue
            value = str(el.value)
            if value[0] == "'" and value[-1] == "'":
                value = value[1:-1]
            value = value.strip()
            child = PTTerminalNode(n.projname, SQL, value, str(el.ttype), i)

            n.HasChild.add(child)
            # print "SQLTToken", el.ttype, el.value
        i+=1
            

def parse_sql(sql, seq, ts, projname, session, user):
    if sql[0] == "'":
        sql = sql[1:-1]
    sql_n = ParseTree(projname, SQL, sql, seq)
    parsed = sqlparse.parse(sql)
    
    for q in parsed:
        visit_sql_pt(q, 0, sql_n)

    return sql_n




"""
PHP Session Parser
"""

class SessionElementType(Enum):
    string = 1
    integer = 2
    array = 3
    empty = 4
    boolean = 5

def skipLeadingBlank(string):
    return lstrip(string)


def skipToNextStringElement(string, element):
    return string[string.index(element):]


def parseSessionContentElementArrayContent(string, projname, pos):
    assert(string[0] == '(')
    rem = string[1:]
    content = dict()
    while(rem[0] != ')'):
        arrayElemIdent, rem = parseSessionContentElement(rem, projname, pos)
        assert(rem[0] == ' ')
        assert(rem[1] == '=')
        assert(rem[2] == '>')
        assert(rem[3] == ' ')
        rem = rem[4:]
        arrayElemContent, rem = parseSessionContentElement(rem, projname, pos)
        content[arrayElemIdent.symbol] = arrayElemContent
    rem = rem[1:]
    return content, rem


def stringToType(string):
    if string == ":INTEGER":
        return SessionElementType.integer
    elif string == ":STRING":
        return SessionElementType.string
    elif string == ":ARRAY":
        return SessionElementType.array
    elif string == ":NIL":
        return SessionElementType.empty
    elif string == ":BOOL":
        return SessionElementType.boolean
    else:
        print "Unknown Session Element Type {}".format(string)
        raise Exception
        

def parseSessionContentElementType(string):
    return [stringToType(string[0:string.index(' ')]),
            string[string.index(' ') + 1:]]
    

def parseSessionContentElement(string, projname, pos):
    string = skipLeadingBlank(string)
    assert string[0] == '(', "expect leading ( for session element"
    type_e, rem = parseSessionContentElementType(string[1:])
    rem = skipLeadingBlank(rem)
    assert(rem[0] == '.')
    assert(rem[1] == ' ')
    if type_e == SessionElementType.empty:
        assert(rem[2] == 'N')
        assert(rem[3] == ')')
        n = PTTerminalNode(projname, PHPSESSION, None, "element-empty", pos)
        return n, rem[4:]
    elif type_e == SessionElementType.string:
        n = PTTerminalNode(projname, PHPSESSION, rem[2:rem.index(')')], "element-string", pos)
        rem = rem[rem.index(')')+1:]
        return n, rem
    elif type_e == SessionElementType.integer:
        integerContent = rem[2:rem.index(')')]
        n = PTTerminalNode(projname, PHPSESSION, integerContent, "element-integer", pos)
        rem = rem[rem.index(')')+1:]
        return [n, rem]
    elif type_e == SessionElementType.boolean:
        booleanContent = rem[2:rem.index(')')]
        n = PTTerminalNode(projname, PHPSESSION, booleanContent, "element-boolean", pos)
        rem = rem[rem.index(')')+1:]
        return [n, rem]
    elif type_e == SessionElementType.array:
        arrayElements, rem = parseSessionContentElementArrayContent(rem[2:], projname, pos)
        rem = skipLeadingBlank(rem)
        assert(rem[0] == ')')
        n = PTNonTerminalNode(projname, PHPSESSION, "element-array", pos)
        for k, v in arrayElements.iteritems():
            kv_n = PTNonTerminalNode(projname, PHPSESSION, "element-array-pair", pos)
            k_n = PTTerminalNode(projname, PHPSESSION, k, "element-key", 0)
            kv_n.HasChild.add(k_n)
            kv_n.HasChild.add(v)
            n.HasChild.add(n)

        return [n, rem[1:]]
    else:
        raise "we should never end up here as it is exhaustive match"


def parseSessionContentIdent(string):
    name = string[0:string.index(' ')]
    rem = string[string.index(' '):]
    return name, rem


def parseSessionContent(string, projname, pos):
    assert(string[0] == '(')
    rem = string
    name, rem = parseSessionContentIdent(string[1:])
    rem = skipLeadingBlank(rem)
    element, rem = parseSessionContentElement(rem, projname, 1)
    rem = skipLeadingBlank(rem)

    sesscnt_n = PTNonTerminalNode(projname, PHPSESSION, "session-content", pos)
    name_n = PTTerminalNode(projname, PHPSESSION, name, "session-content-name", 0)
    sesscnt_n.HasChild.add(name_n)
    sesscnt_n.HasChild.add(element)
    
    return sesscnt_n, rem[1:]


def parseSessionName(string):
    return string[1:string.index(' ')], string[string.index(' '):]


def parseSession(projname, string):
    name, rem = parseSessionName(string)
    rem = skipLeadingBlank(rem)
    contentList = list()
    assert(rem[0] == '(')
    
    rem = skipLeadingBlank(rem[1:])
    assert(rem[0] == '(')
    
    pos = 0
    while(rem[0] != ')'):
        rem = skipLeadingBlank(rem)
        content, rem = parseSessionContent(rem, projname, pos)
        contentList.append(content)
        rem = skipLeadingBlank(rem)
        pos += 1
    assert(rem[2:] == '')

    root = ParseTree(projname, PHPSESSION, string)

    name_n = PTTerminalNode(projname, PHPSESSION, name, "session-name", 0)
    root.HasChild.add(name_n)
    for cont in contentList:
        root.HasChild.add(cont)

    return root
