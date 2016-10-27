from urlparse import urlparse, parse_qs
from datamodel.core import *
from cStringIO import StringIO
import multipart
import re
import sqlparse
import sqlparse.tokens as sqlptokens
from dm_types import *
import Cookie


def parse_url(url, projname):
    scheme, netloc, path, params, query, fragment = urlparse(url)

    url_n = ParseTree(projname, URL)

    pos = 0

    url_n.HasChild.add(PTTerminalNode(projname, URL, scheme, pos))
    pos += 1
    url_n.HasChild.add(PTTerminalNode(projname, URL, netloc, pos))
    pos += 1

    if len(path) > 0:
        url_n.HasChild.add(PTTerminalNode(projname, URL, path, pos))
        pos += 1

    if len(params) > 0:
        url_n.HasChild.add(PTTerminalNode(projname, URL, params, pos))
        pos += 1
    
    if len(query) > 0:
        # Create KeyValuePair
        query_n = PTNonTerminalNode(projname, URL, pos)
        pos += 1
        q_pos = 0  
        for k, vs in parse_qs(query).iteritems():
            for v in vs:
                query_n.HasChild.add(PTTerminalNode(projname, URL, k, q_pos))
                query_n.HasChild.add(PTTerminalNode(projname, URL, v, q_pos+1))
                q_pos += 2
        url_n.HasChild.add(query_n)


        
    if len(fragment) > 0:
        url_n.HasChild.add(PTTerminalNode(projname, URL, fragment, pos))

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
    
    n = ParseTree(projname, dm_type)   

    cookiep = Cookie.SimpleCookie()
    cookiep.load(str(hdrval))

    pos = 0

    for k in cookiep:
        kv_n = PTNonTerminalNode(projname, dm_type, pos)
        k_n  = PTTerminalNode(projname, dm_type, k, 0) 
        v_n  = PTTerminalNode(projname, dm_type, cookiep[k].value, 1)
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

    hdrs_n = PTNonTerminalNode(projname, dm_type, -1)
    pos = 0
    for k, v in hdrs:
        v = v.strip()
        kv_n = PTNonTerminalNode(projname, dm_type, pos)
        k_n  = PTTerminalNode(projname, dm_type, k, 0)
        if k.lower() in ["cookie", "set-cookie"]:
            v_n = parse_cookie(k, v, projname)
            v_n.pos = pos
        elif k.lower() in ["referer", "origin"]:
            v_n = parse_url(v, projname)
            v_n.pos = pos
        else:
            v_n  = PTTerminalNode(projname, dm_type, v, 1)
        
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
        body_n = ParseTree(projname, MULTIPART)
        s_obj = StringIO(body)
        boundary = ctype.split("; boundary=")[1]
        mp = multipart.MultipartParser(s_obj, boundary)
        pos = 0
        for part in mp:
            k = part.options.get("name")
            v = part.value
            kv_n = ParseTree(projname, MULTIPART, pos)
            k_n  = PTTerminalNode(projname, dm_type, k, 0)
            v_n  = PTTerminalNode(projname, dm_type, v, 1)
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
        body_n = ParseTree(projname, ctype)
        s_n = PTTerminalNode(projname, dm_type, body, 0)
        body_n.HasChild.add(s_n)
        return body_n
    
    


def parse_httpreq(method, url, hdrs, body, seq, ts, projname, session, user):
    hreq_n = ParseTree(projname, HTTPREQ)

    hreq_n.HasChild.add(PTTerminalNode(projname, HTTPREQ, method, 0))

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
    hres_n = ParseTree(projname, HTTPRESP)

    hres_n.HasChild.add(PTTerminalNode(projname, HTTPRESP, status, 0))

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
    sel_n = ParseTree(projname, SELENESE)
    
    pairs = [("command", command, 0), ("target", target, 1), ("value", value, 2)]
    for n, v, i in pairs:
        nt = PTNonTerminalNode(projname, SELENESE, i)
        t_n  = PTTerminalNode(projname, SELENESE, n, 0)
        t_v  = PTTerminalNode(projname, SELENESE, v, 1)
        nt.HasChild.add(t_n)
        nt.HasChild.add(t_v)
        sel_n.HasChild.add(nt)

    return sel_n


def visit_sqlast(ast, i, n):
    for el in ast.tokens:
        if el.is_group():
            child = SQLTokenList(n.projname)
            n.Child.add(child)
            # print "SQLTokenList"
            visit_sqlast(el, i+1, child)
        else:
            if el.ttype == sqlptokens.Token.Punctuation or\
               el.ttype == sqlptokens.Token.Text.Whitespace:
                continue
            value = str(el.value)
            if value[0] == "'" and value[-1] == "'":
                value = value[1:-1]
            value = value.strip()
            child = SQLToken(n.projname, str(el.ttype), value)

            n.Child.add(child)
            # print "SQLTToken", el.ttype, el.value
            

def parse_sql(sql, seq, ts, projname, session, user):
    if sql[0] == "'":
        sql = sql[1:-1]
    sql_n = SQLQuery(projname, session, user, seq, ts, sql)
    parsed = sqlparse.parse(sql)
    
    for q in parsed:
        root = SQLStatement(projname, str(q))
        visit_sqlast(q, 1, root)
        sql_n.Statement.add(root)

    return sql_n
