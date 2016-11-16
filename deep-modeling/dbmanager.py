#!/usr/bin/env python
import sys
import argparse

import sqlite3 as lite

from py2neo.database import Graph
from py2neo import watch

import utils.log as log

from api.datamodel.core import *
from api.acquisition import *
from api.dataflow import *
from api.modelabs import *

from shared.config import *

# from dataflow import insert_data_flows
# from modelAbstractor import add_full_abstraction_layer

if DEBUG:
    log.LEVEL = log.LEVELS[-1]
    watch("neo4j.bolt")
else:
    log.LEVEL = log.LEVELS[0]

# global vars are the devils tool
# graph = Graph(host=NEO4J_HOST, user=NEO4J_USERNAME, password=NEO4J_PASSWORD)

# argument parser object
# args_obj = None #global variables are the devils tool

# really dislike havin the setup code here!
UNIQUENESS = [(DFAState.__name__,
                ["uuid"]),
              (DFAStateTransition.__name__,
                ["uuid"]),
              (Event.__name__,
                ["uuid"]),
              (ParseTree.__name__,
                ["uuid"]),
              (PTTerminalNode.__name__,
                ["uuid"]),
              (PTNonTerminalNode.__name__,
                ["uuid"]),
              (Variable.__name__,
                ["uuid"])
              ]

INDEX = [
         (DFAState.__name__,
            ["projname", "dm_type"]),
         (DFAStateTransition.__name__,
            ["projname", "dm_type"]),
         (Event.__name__,
            ["projname", "seq", "session", "user", "dm_type"]),
         (ParseTree.__name__,
            ["projname", "dm_type"]),
         (PTTerminalNode.__name__,
            ["projname", "dm_type"]),
         (PTNonTerminalNode.__name__,
            ["projname", "dm_type"]),
         (Variable.__name__,
            ["projname", "seq", "session", "user", "dm_type"]),
         ]


def load_selcmd_sqlite(fname, logger=None):
    if logger is not None:
        logger.info("Loading Selense commands from vilanoo2/mosgi SQLite db")

    con = lite.connect(fname)
    cmdlist = []
    with con:
        cur = con.cursor()
        rs = cur.execute("SELECT * FROM selenese_commands ORDER BY id")
        cmdlist = list(rs)
    return cmdlist

    
def load_hreqs_sqlite(fname, logger=None):
    if logger is not None:
        logger.info("Loading HTTP requests from vilanoo2/mosgi SQLite db")

    con = lite.connect(fname)
    reqlist = []
    with con:
        cur = con.cursor()
        rs = cur.execute("SELECT * FROM http_requests ORDER BY id")
        reqlist = list(rs)
    return reqlist


def load_hres_sqlite(fname, logger=None):
    if logger is not None:
        logger.info("Loading HTTP responses from vilanoo2/mosgi SQLite db")

    con = lite.connect(fname)
    resplist = []
    with con:
        cur = con.cursor()
        rs = cur.execute("SELECT * FROM http_responses ORDER BY id")
        resplist = list(rs)
    return resplist


def load_cmd2http_sqlite(fname, logger=None):
    if logger is not None:
        logger.info("Loading Selenese command to HTTP requests\
 relationships from vilanoo2/mosgi SQLite db")

    con = lite.connect(fname)
    ids = []
    with con:
        cur = con.cursor()
        rs = cur.execute("SELECT id, command_id FROM http_requests")
        ids = list(rs)
    return ids

def load_xdebug_sqlite(fname, logger=None):
    if logger is not None:
        logger.info("Loading XDEBUG traces from Mosgi SQLite db")

    con = lite.connect(fname)
    ids = []
    with con:
        cur = con.cursor()
        rs = cur.execute("SELECT DISTINCT  http_request_id FROM xdebug_dumps ORDER BY 1 ASC")
        ids = list(rs)
    return ids


def load_queries_sqlite(fname, logger=None):
    if logger is not None:
        logger.info("Loading SQL queries from  Analyzer SQLite db")

    con = lite.connect(fname)
    ids = []
    with con:
        cur = con.cursor()
        rs = cur.execute("SELECT * FROM sql_queries")
        ids = list(rs)
    return ids

def load_php_sessions_dumps(fname, logger=None):
    if logger is not None:
        logger.info("Loading PHP Session dumps from Analyzer SQLite db")

    con = lite.connect(fname)
    ids = []
    with con:
        cur = con.cursor()
        rs = cur.execute("SELECT http_request_id, count(*) FROM sessions GROUP BY 1 ORDER BY 1 ASC")
        ids = list(rs)
    return ids

def load_php_sessions(fname, logger=None):
    if logger is not None:
        logger.info("Loading PHP Sessions from Analyzer SQLite db")

    con = lite.connect(fname)
    ids = []
    with con:
        cur = con.cursor()
        rs = cur.execute("SELECT http_request_id,session_string FROM sessions")
        ids = list(rs)
    return ids

def init_database(args, graph, logger=None):
    if logger is not None:
        logger.info("Initialize database")

    for name, props in UNIQUENESS:
        for p in props:
            try:
                graph.schema.create_uniqueness_constraint(name, p)
            except Exception, e:
                logger.error(e)

    for name, props in INDEX:
        for p in props:
            try:
                graph.schema.create_index(name, p)
            except Exception, e:
                logger.error(e)


def reset_database(args, graph, logger=None):
    if logger is not None:
        logger.info("Deleting nodes and relationships. To COMPLETELY reset\
 the database please use: rm -Rf data/graph.db.\
 WARNING: this will permanently remove data of *ALL* projects")

    for name, props in UNIQUENESS:
        for p in props:
            try:
                graph.schema.drop_uniqueness_constraint(name, p)
            except Exception, e:
                logger.error(e)

    for name, props in INDEX:
        for p in props:
            try:
                graph.schema.drop_index(name, p)
            except Exception, e:
                logger.error(e)

    graph.delete_all()
    logger.info("Done. Too late for regrets.")


def show_stats_database(args, graph, logger=None):
    if logger is not None:
        logger.info("Database statistics")

    stats = graph.run("MATCH (n:Event) RETURN DISTINCT n.projname AS projname, n.session AS session, n.user AS user")
    stats = list(stats)
    print ""
    print "| {:^20} | {:^60} | {:^20} |".format("PROJECT", "SESSION", "USER")
    print "=" * 110
    for s in stats:
        print "| {projname:<20} | {session:<60} | {user:<20} |".format(**s)
    print "\r\n\r\n"


    stats = graph.run("START n=node(*) RETURN distinct labels(n) AS l, count(n) AS c ORDER BY c DESC")

    print "| {:^20} | {:^20} |".format("NODE LABEL", "COUNT")
    print "=" * 47
    for s in stats:
        print "| {:<20} | {:>20} |".format(", ".join(s["l"]), s["c"])
    print "\r\n\r\n"

    stats = graph.run("START n=relationship(*) RETURN distinct type(n) AS t, count(n) AS c ORDER BY c DESC")

    print "| {:^20} | {:^20} |".format("REL.TYPE", "COUNT")
    print "=" * 47
    for s in stats:
        print "| {:<20} | {:>20} |".format(s["t"], s["c"])
    print "\r\n\r\n"

def show_csrf(args, graph, logger=None):

    http_reqs   = graph.run("""MATCH (e:Event {dm_type:"HttpRequest"})
                               RETURN e.projname AS projname, 
                                      e.session AS session, 
                                      e.user AS user, 
                                      count(e) AS http_reqs
                             ORDER BY projname,
                                      session,
                                      user,
                                      http_reqs""")
    http_reqs = list(http_reqs)

    sql   = graph.run("""MATCH (pt:ParseTree {dm_type:"SQLQuery"})-[:PARSES]->(e:Event)
                          WITH DISTINCT e.projname AS projname, e.session AS session, e.user AS user, pt 
                        RETURN projname, 
                               session, 
                               user, 
                               count(pt) AS sql
                      ORDER BY projname,
                               session,
                               user,
                               sql""")
    sql = list(sql)

    http_stchng = graph.run("""MATCH (e:Event {dm_type:"HttpRequest"})-[:CAUSED]->(m:Event)<-[:PARSES]-(s:ParseTree {dm_type:"SQLQuery"}) 
                                 WITH DISTINCT e
                               RETURN e.projname AS projname, 
                                      e.session AS session, 
                                      e.user AS user, 
                                      count(e) AS http_stchng
                             ORDER BY projname,
                                      session,
                                      user,
                                      http_stchng""")
    http_stchng = list(http_stchng)

    stchang_op = graph.run("""MATCH acc=(sym:DFAStateTransition)-[a:ACCEPTS]->(e:Event {dm_type:"HttpRequest"}) 
                     WITH DISTINCT e.projname AS projname, e.session AS session, e.user AS user, sym 
                   RETURN projname, 
                          session, 
                          user, 
                          count(sym) as stchng_op
                 ORDER BY projname,
                          session,
                          user,
                          stchng_op""")

    stchang_op = list(stchang_op)

    df_stchang_op = graph.run("""MATCH acc=(sym:DFAStateTransition)-[a:ACCEPTS]->(e:Event {dm_type:"HttpRequest"}), 
                                        df=(e2:Event)<-[:BELONGS_TO]-(v2:Variable)<-[:PROPAGATES_TO]-(v1:Variable)-[:BELONGS_TO]->(e) 
                         WITH DISTINCT sym.projname AS projname, 
                                       e.session AS session, 
                                       e.user AS user, 
                                       v1 
                                RETURN projname, 
                                       session, 
                                       user, 
                                       count(v1) AS df_stchng_op
                              ORDER BY projname,
                                       session,
                                       user,
                                       df_stchng_op""")

    df_stchang_op = list(df_stchang_op)

    df_big_stchang_op = graph.run("""MATCH acc=(sym:DFAStateTransition)-[a:ACCEPTS]->(e:Event {dm_type:"HttpRequest"}), 
                                           df=(e2:Event)<-[:BELONGS_TO]-(v2:Variable)<-[:PROPAGATES_TO]-(v1:Variable)-[:BELONGS_TO]->(e) 
                                     WHERE size(v1.value) > 10
                             WITH DISTINCT sym.projname AS projname, 
                                           e.session AS session, 
                                           e.user AS user, 
                                           v1 
                                      RETURN projname, 
                                           session, 
                                           user, 
                                           count(v1) AS df_big_stchng_op
                                  ORDER BY projname,
                                           session,
                                           user,
                                           df_big_stchng_op""")

    df_big_stchang_op = list(df_big_stchang_op)

    out = []
    for els in zip(http_reqs, sql, http_stchng, stchang_op, df_stchang_op, df_big_stchang_op):
        aux = dict()
        for e in els:
            aux.update(dict(e))
        out.append(aux)


    print ""
    hdr = "| {:^14} | {:^60} | {:^14} | {:^14} | {:^14} | {:^14} | {:^14} | {:^14} | {:^14}|".format("PROJECT", "SESSION", "USER", "HTTP Reqs.", "SQL Qs.", "HTTP ST.CNG", "ST.CNG OP", "DF ST.CNG OP", "DF ST.CNG OP>10")
    print hdr
    print "=" * len(hdr)
    for s in out:
        print "| {projname:<14} | {session:<60} | {user:<14} | {http_reqs:>14} | {sql:>14} | {http_stchng:>14} | {stchng_op:>14} | {df_stchng_op:>14} | {df_big_stchng_op:>14} |".format(**s)
    print "\r\n\r\n"



def import_all(args, graph, logger=None):
    if logger is not None:
        logger.info("Importing all from vilanoo2/mosgi SQLite Database...")

    import_selenese(args, graph, logger)

    import_http(args, graph, logger)

    import_rel_selhttp(args, graph, logger)

    import_xdebug(args, graph, logger)

    import_sql(args, graph, logger)

    import_session(args, graph, logger)


def import_selenese(args, graph, logger=None):
    cmdlist = load_selcmd_sqlite(args.vilanoo_fname)
    insert_selenese(graph, cmdlist, args.projname,
                                     args.session, args.user, logger)
    

def import_http(args, graph, logger=None):
    hreqs = load_hreqs_sqlite(args.vilanoo_fname)
    hress = load_hres_sqlite(args.vilanoo_fname)
    insert_http(graph, hreqs, hress, args.projname,
                                    args.session, args.user, logger)

    
def import_rel_selhttp(args, graph, logger=None):
    ids = load_cmd2http_sqlite(args.vilanoo_fname)
    insert_causality_selhttp(graph, ids, args.projname,
                                    args.session, args.user, logger)


def import_xdebug(args, graph, logger=None):
    ids = load_xdebug_sqlite(args.mosgi_fname)
    insert_xdebug(graph, ids, args.projname,
                                   args.session, args.user, logger)

def import_sql(args, graph, logger=None):
    ids = load_queries_sqlite(args.analyzer_fname)
    insert_queries(graph, ids, args.projname,
                                   args.session, args.user, logger)


def import_session(args, graph, logger=None):
    sessions = load_php_sessions_dumps(args.analyzer_fname)
    insert_session_dumps(graph, sessions, args.projname,
                                    args.session, args.user, logger)
    sessions = load_php_sessions(args.analyzer_fname)
    insert_sessions(graph, sessions, args.projname,
                                     args.session, args.user, logger)



def analysis_dataflow(args, graph, logger=None):
    insert_variables(graph, args.projname,
                                    args.session, args.user, logger)
    insert_vertical_chains(graph, args.projname,
                                    args.session, args.user, logger)
    insert_backward_selenese_chains(graph, args.projname,
                                    args.session, args.user, logger)

def analysis_data_type_inference(args, graph, logger=None):
    insert_user_generated_chains(graph, args.projname,
                                    args.session, args.user, logger)

def analysis_model_inference(args, graph, logger=None):
    magic_mike(graph, args.projname, args.session, args.user, logger)

def analysis_intracausality(args, graph, logger=None):
    insert_intracausality(graph, args.projname, args.session, args.user, logger)

def analysis_all(args, graph, logger=None):
    analysis_dataflow(args, graph, logger)
    analysis_data_type_inference(args, graph, logger)
    analysis_model_inference(args, graph, logger)
    analysis_intracausality(args, graph, logger)

def parse_args(args):
    p = argparse.ArgumentParser(description='dbmanager parameters')
    subp = p.add_subparsers()

    init_p = subp.add_parser("init", help="Initialize the database")
    init_p.set_defaults(func=init_database)

    reset_p = subp.add_parser("reset", help="Reset the database")
    reset_p.set_defaults(func=reset_database)

    """
    ==========
    STATISTICS
    ==========
    """

    stats_p = subp.add_parser("stats", help="Show statistics: project names, sessions, and users")
    stats_p.set_defaults(func=show_stats_database)    

    stats_p = subp.add_parser("csrf", help="Show CSRF data")
    stats_p.set_defaults(func=show_csrf) 

    """
    ========
    ANALYSIS
    ========
    """

    an_p = subp.add_parser("analysis", help="Analyze existing deep models")
    an_subp = an_p.add_subparsers()

    an_all = an_subp.add_parser("all", help="Perform all analyses")
    an_all.add_argument("projname", help="Project name")
    an_all.add_argument("session",  help="Session identifier")
    an_all.add_argument("user",     help="User identifier")
    an_all.set_defaults(func=analysis_all)


    """
    Data propagation
    """

    an_df = an_subp.add_parser("dataflow", help="Create the data flow model")
    an_df.add_argument("projname", help="Project name")
    an_df.add_argument("session",  help="Session identifier")
    an_df.add_argument("user",     help="User identifier")
    an_df.set_defaults(func=analysis_dataflow)

    """
    Data propagation type inference
    """

    an_df = an_subp.add_parser("datatype", help="Infere data types (syn, sem, and prop)")
    an_df.add_argument("projname", help="Project name")
    an_df.add_argument("session",  help="Session identifier")
    an_df.add_argument("user",     help="User identifier")
    an_df.set_defaults(func=analysis_data_type_inference)

    """
    Model inference
    """
    an_inference = an_subp.add_parser("inference", help="Infer DFA/NFA models")
    an_inference.add_argument("projname", help="Project name")
    an_inference.add_argument("session",  help="Session identifier")
    an_inference.add_argument("user",     help="User identifier")
    an_inference.set_defaults(func=analysis_model_inference)

    """
    Intra-causality
    """
    an_inference = an_subp.add_parser("intracaus", help="Adjust causality according to Referer")
    an_inference.add_argument("projname", help="Project name")
    an_inference.add_argument("session",  help="Session identifier")
    an_inference.add_argument("user",     help="User identifier")
    an_inference.set_defaults(func=analysis_intracausality)

    

    """
    =============
    IMPORT TRACES
    =============
    """
    imp_p = subp.add_parser("import", help="Import data")
    imp_subp = imp_p.add_subparsers()

    """
    Import all
    """

    imp_all_p = imp_subp.add_parser("all",   help="Import all data into Neo4j")
    imp_all_p.add_argument("vilanoo_fname",  help="Vilanoo2 SQLite3 database filename")
    imp_all_p.add_argument("mosgi_fname",    help="Mosgi SQLite3 database")
    imp_all_p.add_argument("analyzer_fname", help="Analyzer SQLite3 database")
    imp_all_p.add_argument("projname",       help="Project name")
    imp_all_p.add_argument("session",        help="Session identifier")
    imp_all_p.add_argument("user",           help="User identifier")
    imp_all_p.set_defaults(func=import_all)

    """
    Import Selenese
    """

    imp_sel_p = imp_subp.add_parser("selenese", help="Import Selenese PTs/trace from vilanoo2 SQLite3 database")
    imp_sel_p.add_argument("vilanoo_fname",     help="Vilanoo2 SQLite3 database filename")
    imp_sel_p.add_argument("projname",          help="Project name")
    imp_sel_p.add_argument("session",           help="Session identifier")
    imp_sel_p.add_argument("user",              help="User identifier")
    imp_sel_p.set_defaults(func=import_selenese)

    """
    HTTP
    """

    imp_sel_p = imp_subp.add_parser("http", help="Import HTTP PTs/trace from\
 vilanoo2 SQLite3 database")
    imp_sel_p.add_argument("vilanoo_fname", help="Vilanoo2 SQLite3\
 database filename")
    imp_sel_p.add_argument("projname", help="Project name")
    imp_sel_p.add_argument("session",  help="Session identifier")
    imp_sel_p.add_argument("user",     help="User identifier")
    imp_sel_p.set_defaults(func=import_http)

    """
    Causality HTTP and Selenese
    """

    imp_sel_p = imp_subp.add_parser("rel_selhttp", help="Import causality\
 between Selenese and HTTP events from from vilanoo2\
 SQLite3 database")
    imp_sel_p.add_argument("vilanoo_fname", help="Vilanoo2 SQLite3\
 database filename")
    imp_sel_p.add_argument("projname", help="Project name")
    imp_sel_p.add_argument("session",  help="Session identifier")
    imp_sel_p.add_argument("user",     help="User identifier")
    imp_sel_p.set_defaults(func=import_rel_selhttp)

    """
    XDebug and Causality HTTP->XDEBUG
    """

    imp_sel_p = imp_subp.add_parser("xdebug", help="Import XDEBUG traces\
 from Analyzer SQLite3 database")
    imp_sel_p.add_argument("mosgi_fname", help="Mosgi SQLite3 database")
    imp_sel_p.add_argument("projname", help="Project name")
    imp_sel_p.add_argument("session",  help="Session identifier")
    imp_sel_p.add_argument("user",     help="User identifier")
    imp_sel_p.set_defaults(func=import_xdebug)

    """
    SQL
    """

    imp_sel_p = imp_subp.add_parser("sql", help="Import SQL PTs\
 from Analyzer SQLite3 database")
    imp_sel_p.add_argument("analyzer_fname", help="Analyzer SQLite3\
 database filename")
    imp_sel_p.add_argument("projname", help="Project name")
    imp_sel_p.add_argument("session",  help="Session identifier")
    imp_sel_p.add_argument("user",     help="User identifier")
    imp_sel_p.set_defaults(func=import_sql)

    """
    PHP Session
    """

    imp_sel_p = imp_subp.add_parser("phpsession", help="Import PHPSessions Trace and PTs\
 from Analyzer SQLite3 database")
    imp_sel_p.add_argument("analyzer_fname", help="Analyzer SQLite3\
 database filename")
    imp_sel_p.add_argument("projname", help="Project name")
    imp_sel_p.add_argument("session",  help="Session identifier")
    imp_sel_p.add_argument("user",     help="User identifier")
    imp_sel_p.set_defaults(func=import_session)




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
