from urlparse import urlparse, parse_qs
from datamodel.core import *
from datamodel.selenese import *
from datamodel.http import *
from datamodel.sql import *
# from core import *
from cStringIO import StringIO
import multipart
import re
import sqlparse
import sqlparse.tokens as sqlptokens


def parse_url(url, projname):
    scheme, netloc, path, params, query, fragment = urlparse(url)
    url_n = URL(projname, url)
    url_n.Scheme.add(DataValue(projname, scheme))
    url_n.Netloc.add(DataValue(projname, netloc))

    if len(path) > 0:
        url_n.Path.add(DataValue(projname, scheme))

    if len(params) > 0:
        url_n.Params.add(DataValue(projname, params))
    
    if len(query) > 0:
        # Create KeyValuePair
        for k, vs in parse_qs(query).iteritems():
            for v in vs:
                p = KeyValuePair(projname, k, v)
                url_n.QueryString.add(p)
        
    if len(fragment) > 0:
        url_n.Fragment.add(DataValue(projname, fragment))

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


def parse_headers(hdrs, projname):
    hdrs = headers_to_list(hdrs)

    def keep(h):
        for r in IGNORE_HEADERS:
            if re.match(r, h[0]) is not None:
                return False
        return True
    hdrs = filter(keep, hdrs)  # Remove blacklisted headers

    hdrs_n = HeaderList(projname)
    for k, v in hdrs:
        v = v.strip()
        kv_n = KeyValuePair(projname, k, v)
        hdrs_n.Header.add(kv_n)
    
    return hdrs_n


def content_type(hdrs):
    hdrs = headers_to_list(hdrs)
    for k, v in hdrs:
        if k.lower() == "content-type":
            return v
    return None


def parse_body(body, ctype, projname):
    body_n = Body(projname, ctype)
    if "multipart/form-data" in ctype:
        # parse multipart/form-data body
        s_obj = StringIO(body)
        boundary = ctype.split("; boundary=")[1]
        mp = multipart.MultipartParser(s_obj, boundary)
        for part in mp:
            k = part.options.get("name")
            v = part.value
            kv_n = KeyValuePair(projname, k, v)
            body_n.Contains.add(kv_n)

    elif "application/x-www-form-urlencoded" in ctype:
        pass
        # parse application/x-www-form-urlencoded body
        # for k, vs in parse_qs(query).iteritems():  # TODO:query is undefined
        #    for v in vs:
        #        p = KeyValuePair(projname, k, v)
        #        body_n.Contains.add(p)
    else:
        # No JSON yet...
        s_n = DataValue(projname, body)
        body_n.Contains.add(s_n)
    
    return body_n


def parse_httpreq(method, url, hdrs, body, seq, ts, projname, session, user):
    hreq_n = HTTPRequest(projname, session, user, seq, ts, method, url)

    url_n = parse_url(url, projname)
    hreq_n.URL.add(url_n)

    hdrs_n = parse_headers(hdrs, projname)
    hreq_n.Header.add(hdrs_n)

    ctype = content_type(hdrs)
    if ctype:
        body_n = parse_body(body, ctype, projname)
        hreq_n.Body.add(body_n)

    return hreq_n


def parse_httpres(status, hdrs, body, seq, ts, projname, session, user):
    hres_n = HTTPResponse(projname, session, user, seq, ts, status)

    hdrs_n = parse_headers(hdrs, projname)
    hres_n.Header.add(hdrs_n)

    ctype = content_type(hdrs)
    if ctype:
        body_n = parse_body(body, ctype, projname)
        hres_n.Body.add(body_n)

    return hres_n


def parse_selcmd(command, target, value, seq, ts, projname, session, user):
    cmd_n = SeleneseCommand(projname, session, user, seq,
                                ts, command, target, value)
    cmd_n.Command.add(DataValue(projname, command))
    cmd_n.Target.add(DataValue(projname, target))
    cmd_n.Value.add(DataValue(projname, value))

    return cmd_n


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
