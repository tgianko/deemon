#!/usr/bin/env python
import sys
import argparse
import json
import urlparse
import sqlite3 as lite
import os
import re
import datetime
from py2neo.database import Graph
from py2neo import watch
from Cookie import SimpleCookie
from urllib import urlencode

import api.dm_types as dm_types
import utils.log as log
from shared.config import *
from api.datamodel.core import *
from api.multipart import Multipart
import string

DEBUG = False

if DEBUG:
    log.LEVEL = log.LEVELS[-1]
    watch("neo4j.bolt")
else:
    log.LEVEL = log.LEVELS[0]

logger        = log.getdebuglogger("tester")
sqlite_schema = os.path.join(os.getcwd(), "../data/DBSchemaCSRFTests.sql")

#
# Credits http://stackoverflow.com/questions/20248355/how-to-get-python-to-gracefully-format-none-and-non-existing-fields
#
class PartialFormatter(string.Formatter):
    def __init__(self, missing='n.a.', bad_fmt='err.'):
        self.missing, self.bad_fmt=missing, bad_fmt

    def get_field(self, field_name, args, kwargs):
        # Handle a key not found
        try:
            val=super(PartialFormatter, self).get_field(field_name, args, kwargs)
            # Python 3, 'super().get_field(field_name, args, kwargs)' works
        except (KeyError, AttributeError):
            val=None,field_name 
        return val 

    def format_field(self, value, spec):
        # handle an invalid format
        if value==None: 
            value = self.missing
        try:
            return super(PartialFormatter, self).format_field(value, spec)
        except ValueError:
            if self.bad_fmt is not None: return self.bad_fmt   
            else: raise


def test_stats(args, graph, logger=None):
    logger.info("Retrieving max_reqs")
    query = """MATCH (e:Event {dm_type:"HttpRequest"}) 
                WITH DISTINCT e.projname AS projname, left(e.session,size(e.session)-3) AS operation, 
                              e.user AS user, 
                              e.session AS session, 
                              count(e) AS http_reqs 
              RETURN projname, 
                     operation, 
                     max(http_reqs) AS max_reqs
               ORDER BY projname, 
                        operation;"""

    max_reqs = graph.run(query)

    logger.info("Retrieving max_stchreqs")
    query = """MATCH (e:Event {dm_type:"HttpRequest"})-[:CAUSED]->(m:Event)<-[:PARSES]-(s:ParseTree {dm_type:"SQLQuery"}) 
                WITH DISTINCT e 
                WITH DISTINCT e.projname AS projname, 
                              e.session AS session, 
                              e.user AS user, 
                              left(e.session, size(e.session)-3) AS operation, 
                              count(e) AS tot_stchreqs 
              RETURN projname, 
                     operation, 
                     max(tot_stchreqs) AS max_stchreqs
               ORDER BY projname, 
                        operation;"""

    max_stchreqs = graph.run(query)

    logger.info("Retrieving max_su_vars")
    query = """MATCH stch=(e:Event {dm_type:"HttpRequest"})-[:CAUSED]->(m:Event)<-[:PARSES]-(s:ParseTree {dm_type:"SQLQuery"}), 
                       (v1:Variable)-[:BELONGS_TO]->(e) 
               WHERE "session_unique" IN v1.semtype 
                WITH DISTINCT e, v1 
                WITH DISTINCT e.projname AS projname, 
                              e.session AS session, 
                              e.user AS user, 
                              left(e.session, size(e.session)-3) AS operation, 
                              count(v1) AS tot_vars 
              RETURN projname, 
                     operation, 
                     max(tot_vars) AS max_su_vars
               ORDER BY projname, 
                        operation;"""

    max_su_vars = graph.run(query)


    logger.info("Retrieving max_pchains")
    query = """MATCH stch=(e:Event {dm_type:"HttpRequest"})-[:CAUSED]->(m:Event)<-[:PARSES]-(s:ParseTree {dm_type:"SQLQuery"}), 
                       df=(e2:Event)<-[:BELONGS_TO]-(v2:Variable)<-[:PROPAGATES_TO]-(v1:Variable)-[:BELONGS_TO]->(e) 
                WITH DISTINCT e, v1
                WITH DISTINCT e.projname AS projname, 
                              e.session AS session, 
                              e.user AS user, 
                              left(e.session, size(e.session)-3) AS operation, 
                              count(v1) AS tot_vars 
              RETURN projname, 
                     operation, 
                     max(tot_vars) AS max_pchains
               ORDER BY projname, 
                        operation;"""

    max_pchains = graph.run(query)

    logger.info("Retrieving max_pchains_su")
    query = """MATCH stch=(e:Event {dm_type:"HttpRequest"})-[:CAUSED]->(m:Event)<-[:PARSES]-(s:ParseTree {dm_type:"SQLQuery"}), 
                       df=(e2:Event)<-[:BELONGS_TO]-(v2:Variable)<-[:PROPAGATES_TO]-(v1:Variable)-[:BELONGS_TO]->(e) 
               WHERE "session_unique" IN v1.semtype
                WITH DISTINCT e, v1 
                WITH DISTINCT e.projname AS projname, 
                              e.session AS session, 
                              e.user AS user, 
                              left(e.session, size(e.session)-3) AS operation, 
                              count(v1) AS tot_vars 
              RETURN projname, 
                     operation, 
                     max(tot_vars) AS max_pchains_su
               ORDER BY projname, 
                        operation;"""

    max_pchains_su = graph.run(query)



    out = []
    for els in map(None, max_reqs, max_stchreqs, max_su_vars, max_pchains, max_pchains_su):
        aux = dict()
        for e in els:
            if e is not None:
                aux.update(dict(e))
            else:
                aux.update({})
        out.append(aux)

    fmt=PartialFormatter()

    col_names = ("PROJECT", "SESSION", "Max Reqs.", "Max St.Ch. Reqs", "Max SU Vars", "Max PChains", "Max SU PChains")
    hdr = "| {:^14} | {:^60} | {:^18} | {:^18} | {:^18} | {:^18} | {:^18} |".format(*col_names)
    print hdr
    print "=" * len(hdr)
    for s in out:
        print fmt.format("| {projname:<14} | {operation:<60} | {max_reqs:>18} | {max_stchreqs:>18} | {max_su_vars:>18} | {max_pchains:>18} | {max_pchains_su:>18} |", **s)
    print "\r\n\r\n"


    # rs = graph.run("""MATCH acc=(sym:DFAStateTransition)-[a:ACCEPTS]->(e:Event {dm_type:"HttpRequest"}), 
    #                          df=(e2:Event)<-[:BELONGS_TO]-(v2:Variable)<-[:PROPAGATES_TO]-(v1:Variable)-[:BELONGS_TO]->(e), 
    #                          pt=(p:ParseTree)-[:PARSES]->(e) 
    #           WITH DISTINCT sym.uuid AS uuid, 
    #                         sym.projname AS projname, 
    #                         e.session AS session, 
    #                         e, 
    #                         p 
    #                  RETURN projname, 
    #                         session, 
    #                         uuid, 
    #                         count(e) as reqs
    #                ORDER BY projname, 
    #                         session, 
    #                         reqs""")

    # print ""
    # hdr = "| {:^14} | {:^60} | {:^36} | {:^14} |".format("PROJECT", "SESSION", "State Ch. OP UUID", "# HTTP Reqs.")
    # print hdr
    # print "=" * len(hdr)
    # for s in rs:
    #     print "| {projname:<14} | {session:<60} | {uuid:<36} | {reqs:>14} |".format(**s)
    print "\r\n\r\n"

def skip(n, ignore):
    if ignore is not None and ignore == n:
        logger.warning("Skipping {}={}".format(n.s_type, n.symbol))
        #print "Skipping", ignore.uuid, ignore.symbol
        return True
    return False

def pt_to_url(pt, ignore=None):
    scheme, netloc, path, qs = "", "", "", ""
    for child in sorted(list(pt.HasChild), key=lambda c: c.pos):
        if isinstance(child, PTTerminalNode):
            if child.s_type == "path":
                path = child.symbol
            elif child.s_type == "scheme":
                scheme = child.symbol
            elif child.s_type == "netloc":
                netloc = child.symbol
            else:
                raise Exception("Unhandled URL component {} {}".format(child.uuid, child.s_type))
        if isinstance(child, PTNonTerminalNode):
            if child.s_type == "query-string":
                qs = pt_to_query(child, ignore)
            else:
                raise Exception("Unhandled NonTerminal component {} {}".format(child.uuid, child.s_type))

    return urlparse.urlunparse((scheme, netloc, path, "", qs, ""))

def pt_to_query(pt, ignore=None):
    qs = {}
    parname = None
    for child in sorted(list(pt.HasChild), key=lambda c: c.pos):
        #logger.info(" --- {} {}".format(child.s_type, child.symbol))
        if child.s_type == "param-name":
            if skip(child, ignore):
                parname = None
            elif parname is None:
                parname = child.symbol
            else:
                qs.setdefault(parname, []).append("")
                parname = None
        elif child.s_type == "param-value":
            if skip(child, ignore):
                parname = None
            elif parname is None:
                #raise Exception("Ooops. Two param-values in a row? {} {}".format(child.s_type, child.symbol))
                logger.warning("Ooops. Two param-values in a row? {} {}".format(child.s_type, child.symbol))
            else:
                qs.setdefault(parname, []).append(child.symbol)
                parname = None
        else:
            Exception("Mmmh... neither param-name nor param-value. This query string is really messed up: {}".format(child.s_type))
    return urlencode(qs, True)

def pt_to_urlformenc(pt, ignore=None):
    qs = {}
    for child in pt.HasChild:
        name, value = sorted(list(child.HasChild), key=lambda c: c.pos)
        if skip(value, ignore):
            qs.setdefault(name.symbol, [])
        else:
            qs.setdefault(name.symbol, []).append(value.symbol)            
    return urlencode(qs, True)

def inline_cookie(cookie):
    """Return an inline cookie string"""
    result = []
    items = cookie.items()
    items.sort()
    for K,V in items:
        result.append( V.OutputString() )
    return "; ".join(result)

def pt_to_cookie(pt, ignore=None):
    cookie = SimpleCookie()
    for child in pt.HasChild:
        name, value = sorted(list(child.HasChild), key=lambda c: c.pos)
        if skip(name, ignore):
            pass
        else:
            if skip(value, ignore):
                pass
            else:
                cookie[str(name.symbol)]= str(value.symbol)
    return inline_cookie(cookie)

def pt_to_json(pt, ignore=None):
    return json.dumps(visit_pt_json(pt, ignore=ignore))

def visit_pt_json(pt, ignore=None):
    if isinstance(pt, PTNonTerminalNode):
        if pt.s_type == "json-object":
            obj = dict()
            for child in pt.HasChild:
                k = pt_to_json(child, ignore=ignore)
                v = pt_to_json(child, ignore=ignore)
                obj[k] = v
            return obj
        elif pt.s_type == "json-array":
            arr = [pt_to_json(child, ignore) for child in pt.HasChild]
            return arr
    elif isinstance(pt, PTTerminalNode):
        if pt.s_type == "json-string":
            if not skip(pt, ignore):
                return str(pt.symbol)
        elif pt.s_type == "json-number-int":
            if not skip(pt, ignore):
                return int(pt.symbol)
        elif pt.s_type == "json-number-real":
            if not skip(pt, ignore):
                return float(pt.symbol)
        elif pt.s_type == "json-number-bool":
            if not skip(pt, ignore):
                return bool(pt.symbol)
        elif pt.s_type == "json-number-null":
            if not skip(pt, ignore):
                return None
    else:
        return list(pt.HasChild)[0]


def pt_to_body(pt, ignore=None):
    ct, body = "", ""
    if pt.dm_type == dm_types.MULTIPART:
        mp = Multipart()
        for child in pt.HasChild:
            name, value = sorted(list(child.HasChild), key=lambda c: c.pos)
            if not skip(child, ignore):
                mp.field(name.symbol, value.symbol)
        ct, body = mp.get()
    elif pt.dm_type == dm_types.JSON:
        ct = ""
        body = pt_to_json(pt, ignore=ignore)
    elif pt.dm_type == dm_types.FORMURLENC:
        ct = ""
        body = pt_to_urlformenc(pt, ignore=ignore)
    else:
        ct = ""
        body = pt.message
    return ct, body

def pt_to_req(pt, ignore=None):
    command = ""
    url = ""
    headers = {}
    ct = ""
    body = None

    Q = list(pt.HasChild)
    while len(Q) > 0:
        child = Q.pop()

        if isinstance(child, PTNonTerminalNode):
            # Special case: we just pick name and value
            if child.s_type == "header-field":
                name, value = sorted(list(child.HasChild), key=lambda c: c.pos)

                k, v = "", ""
                # Adjustment because we screwed up with the pos for URLs
                if isinstance(name, ParseTree):
                    name, value = value, name # SWAAAAP!

                if isinstance(value, ParseTree):
                    if value.dm_type == dm_types.URL:
                        v = pt_to_url(value, ignore=ignore)
                    elif value.dm_type == dm_types.COOKIE:
                        v = pt_to_cookie(value, ignore=ignore)
                    else:
                        raise Exception("Unhandled ParseTree {} {}".format(value.uuid, value.dm_type))
                else:
                    if not skip(value, ignore):
                        v = value.symbol
                
                k = name.symbol

                headers.setdefault(k, "")
                if isinstance(v, list):
                    headers[k] = ", ".join(v)
                    #print len(v), v, headers[k]
                else:
                    headers[k] = v
            else:
                Q.extend(child.HasChild)

        elif isinstance(child, PTTerminalNode):
            if child.s_type == "method":
                command = child.symbol
            else:
                raise Exception("Unhandled s_type {} {}".format(child.uuid, child.symbol))
        elif isinstance(child, ParseTree):
            if child.dm_type == dm_types.URL:
                url = pt_to_url(child, ignore=ignore)
            elif child.dm_type in dm_types._BODY:
                ct, body = pt_to_body(child, ignore=ignore)
            else:
                raise Exception("Unhandled ParseTree {} {}".format(child.uuid, child.dm_type))
        else:
            raise Exception("Unhandled Situation {} {}".format(child.uuid, child.dm_type))
   
    if len(ct) > 0: # we have a content type coming from body functions, we need to remove existing ones and replace
        if "content-type" in headers:
            headers["content-type"] = ct
    
    return command, url, headers, body


def sqlitedb_init(filename):
    # If the DB does not exist, lite.connect does not create a folder. 
    # Check folder first...
    dirname = os.path.dirname(filename)
    if len(dirname) > 0 and not os.path.exists(dirname):
        logger.info("Folder {0} does not exist. Creating...".format(dirname))
        os.makedirs(dirname)


    #if not os.path.exists(sqlite_schema):
    #    v_logger.fatal("Houston, we have a problem. sqlite_schema {0} does not exist.".format(sqlite_schema))

    if not os.path.exists(filename):
        logger.info("SQLite DB file {0} does not exist. Creating from {1}".format(filename, sqlite_schema))
        
        f = open(sqlite_schema)
        con = lite.connect(filename)
        with con:            
            cur = con.cursor()
            with f:
                schema = f.read()
                cur.executescript(schema)
        logger.info("SQLite DB file {0} created.".format(filename))

def store_httpreq(seq_id, uuid_request, uuid_tn, uuid_src_var, uuid_sink_var, method, url, headers, body, dbname):
    headers = json.dumps(headers)
              
    con = lite.connect(dbname) 
    con.text_factory = str       
    with con:            
        cur = con.cursor()            
        ##inserting the http_request that triggered the sql_queries            
        data = (seq_id, datetime.datetime.now(), uuid_request, uuid_tn, uuid_src_var, uuid_sink_var, method, url, headers, body)
        cur.execute("INSERT INTO CSRF_tests (seq_id, time, uuid_request, uuid_tn, uuid_src_var, uuid_sink_var, method, url, headers, body) VALUES(?,?,?,?,?,?,?,?,?,?)",
                    data)
        req_id = cur.lastrowid

    return req_id


def test_br_pchain(args, graph, logger=None):
    sqlitedb_init(args.database)

    data = {"len": args.len, "projname": args.projname, "session": args.session}
    uuids = graph.run("""MATCH acc=(sym:DFAStateTransition {projname:{projname}})-[a:ACCEPTS]->(e1:Event {dm_type:"HttpRequest", session:{session}}), 
                                df=(e2:Event)<-[:BELONGS_TO]-(v2:Variable)<-[:PROPAGATES_TO]-(v1:Variable)-[:BELONGS_TO]->(e1), 
                                pt=(p1:ParseTree)-[:PARSES]->(e1), injpt=(p1)-[:HAS_CHILD*]->(tn1:PTTerminalNode)<-[:HAS_VALUE]-(v1) 
                         WHERE size(v1.value) > {len}
                 WITH DISTINCT sym, e1, p1, v1, v2, collect(v2) AS dests, tn1 
                        RETURN sym.uuid, e1.uuid, e1.dm_type, p1.uuid, v1.uuid, v2.uuid, dests, tn1.uuid
                      ORDER BY e1.symbol""", data)
    uuids = list(uuids)
    print "Total number of test cases to generate: {}".format(len(uuids))
    i = 1
    for res in uuids:
       
        pt = ParseTree.select(graph).where(uuid=res["p1.uuid"]).first()
        tn = PTTerminalNode.select(graph).where(uuid=res["tn1.uuid"]).first()
    
        logger.info( "Exporting test case {}/{} by removing {}={}".format(i, len(uuids), tn.s_type, tn.symbol))        

        command, url, headers, body = pt_to_req(pt, tn)
        store_httpreq(i, res["e1.uuid"], res["tn1.uuid"], res["v1.uuid"], res["v2.uuid"], command, url, headers, body, args.database)

        i+=1

def test_sessuniq_brchain(args, graph, logger=None):
    sqlitedb_init(args.database)

   
    data = {"len": args.len, "projname": args.projname, "session": args.session}
    uuids = graph.run("""MATCH acc=(sym:DFAStateTransition {projname:{projname}})-[a:ACCEPTS]->(e1:Event {dm_type:"HttpRequest", session:{session}}), 
                                df=(e2:Event)<-[:BELONGS_TO]-(v2:Variable)<-[:PROPAGATES_TO]-(v1:Variable)-[:BELONGS_TO]->(e1), 
                                pt=(p1:ParseTree)-[:PARSES]->(e1), injpt=(p1)-[:HAS_CHILD*]->(tn1:PTTerminalNode)<-[:HAS_VALUE]-(v1) 
                         WHERE size(v1.value) > {len}
                 WITH DISTINCT sym, e1, p1, v1, v2, collect(v2) AS dests, tn1 
                        RETURN sym.uuid, e1.uuid, e1.dm_type, p1.uuid, v1.uuid, v2.uuid, dests, tn1.uuid
                      ORDER BY e1.symbol""", data)
    uuids = list(uuids)
    print "Total number of test cases to generate: {}".format(len(uuids))
    i = 1
    for res in uuids:
       
        pt = ParseTree.select(graph).where(uuid=res["p1.uuid"]).first()
        tn = PTTerminalNode.select(graph).where(uuid=res["tn1.uuid"]).first()
    
        logger.info( "Exporting test case {}/{} by removing {}={}".format(i, len(uuids), tn.s_type, tn.symbol))        

        command, url, headers, body = pt_to_req(pt, tn)
        store_httpreq(i, res["e1.uuid"], res["tn1.uuid"], res["v1.uuid"], res["v2.uuid"], command, url, headers, body, args.database)

        i+=1
    
def parse_args(args):
    p = argparse.ArgumentParser(description='tester parameters')
    subp = p.add_subparsers()



    stats_p = subp.add_parser("stats", help="Get some statistics about the testing phase") 
    stats_p.set_defaults(func=test_stats) 

    """
    =====
    TESTS
    =====
    """

    tests_p = subp.add_parser("tgen", help="Test case generator functions") 
    tests_subp = tests_p.add_subparsers()
    negltok_p = tests_subp.add_parser("br_pchain", help="Generate a test by breaking a propagation chain") 
    negltok_p.add_argument("len",      help="Minimum value length", type=int)
    negltok_p.add_argument("projname", help="Project name")
    negltok_p.add_argument("session",  help="Session")
    negltok_p.add_argument("database",  help="Database where to store HTTP requests")
    negltok_p.set_defaults(func=test_br_pchain) 
    return p.parse_args(args)

def main(args):
    # global args_obj # global variables are the devils tool
    logger = log.getdebuglogger("dbmanager")
    graph = Graph(host=NEO4J_HOST, user=NEO4J_USERNAME,
                  password=NEO4J_PASSWORD)
    args_obj = parse_args(args)

    args_obj.func(args_obj, graph, logger)


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))


"""
Additional indexes:
:DataValue(value)
:KeyValuePair(value)
:SQLToken(value)
"""
