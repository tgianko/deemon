#!/usr/bin/python2
import sys
import argparse
import utils.log as log
import sqlite3 as lite
from py2neo.database import Graph
from ast.parsers import *
from ast.core import *


NEO4J_HOST      = "localhost"
NEO4J_USERNAME = "neo4j"
NEO4J_PASSWORD = "seesurf"

graph = Graph(host=NEO4J_HOST, user=NEO4J_USERNAME, password=NEO4J_PASSWORD)

DEBUG     = False
VERBOSITY = 1


if DEBUG:
    log.LEVEL = log.LEVELS[-1]
else:
    log.LEVEL = log.LEVELS[0]

# Installing two loggers
logger    = log.getdebuglogger("dbmanager")

# argument parser object
args_obj = None

UNIQUENESS = [SeleneseCommand.__name__,
         HTTPRequest.__name__,
         HTTPResponse.__name__,
         SQLQuery.__name__]

    
INDEX = [(SeleneseCommand.__name__, ["projname", "seq", "session", "user"]),
         (HTTPRequest.__name__,     ["projname", "seq", "session", "user"]),
         (HTTPResponse.__name__,    ["projname", "seq", "session", "user"]),
         (SQLQuery.__name__,        ["projname", "seq", "session", "user"])]

def init_database(args):
    logger.info("Initialize database")

    for name in UNIQUENESS:
        try:
            graph.schema.create_uniqueness_constraint(name, "uuid")
        except Exception, e:
            logger.error(e)

    for name, props in INDEX:
        for p in props:
            try:
                graph.schema.create_index(name, p)
            except Exception, e:
                logger.error(e)



def reset_database(args):
    logger.info("Deleting nodes and relationships. To COMPLETELY reset the database please use: rm -Rf data/graph.db. WARNING: this will permanently remove data of *ALL* projects")

    for name in UNIQUENESS:
        try:
            graph.schema.drop_uniqueness_constraint(name, "uuid")
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

def load_selcmd_sqlite(fname):
    logger.info("Loading Selense commands from vilanoo2/mosgi SQLite db")
    con = lite.connect(fname)    
    cmdlist=[]    
    with con:            
        cur = con.cursor()            
        rs = cur.execute("SELECT * FROM selenese_commands ORDER BY id")
        cmdlist = list(rs)
    return cmdlist
    
def load_hreqs_sqlite(fname):
    logger.info("Loading HTTP requests from vilanoo2/mosgi SQLite db")
    con = lite.connect(fname)
    reqlist = []
    with con:            
        cur = con.cursor()            
        rs = cur.execute("SELECT * FROM http_requests ORDER BY id")
        reqlist = list(rs)
    return reqlist

def load_hres_sqlite(fname):
    logger.info("Loading HTTP responses from vilanoo2/mosgi SQLite db")
    con = lite.connect(fname)        
    resplist = []
    with con:            
        cur = con.cursor()
        rs = cur.execute("SELECT * FROM http_responses ORDER BY id")
        resplist = list(rs)
    return resplist

def load_cmd2http_sqlite(fname):
    logger.info("Loading Selenese command to HTTP requests relationships from vilanoo2/mosgi SQLite db")
    con = lite.connect(fname)
    ids = []    
    with con:            
        cur = con.cursor()            
        rs = cur.execute("SELECT id, command_id FROM http_requests")
        ids = list(rs)
    return ids

def load_queries_sqlite(fname):
    logger.info("Loading SQL queries from  Analyzer SQLite db")
    con = lite.connect(fname)
    ids = []    
    with con:            
        cur = con.cursor()            
        rs = cur.execute("SELECT * FROM sql_queries")
        ids = list(rs)
    return ids

def insert_selenese_commands(cmdlist, projname, session, user):
    logger.info("Importing {} commands...".format(len(cmdlist)))
    # just in case, we order by ID.
    cmdlist = sorted(cmdlist, key=lambda cmd:cmd[0]) 

    prev_cmd_n = None
    for cmd in cmdlist:
        logger.info("Processing Command ID {} / {}".format(cmd[0], len(cmdlist)))
        cmd_n = parse_selcmd(cmd[2], cmd[3], cmd[4], cmd[0], None, projname, session, user)  

        if prev_cmd_n:
            prev_cmd_n.Next.add(cmd_n)
            graph.push(prev_cmd_n)
        
        graph.push(cmd_n)        

        prev_cmd_n = cmd_n

def insert_httpreqs(reqlist, projname, session, user):
    logger.info("Importing {} HTTP requests...".format(len(reqlist)))
    # just in case, we order by ID.
    reqlist = sorted(reqlist, key=lambda r:r[0]) 
    
    prev_hreq_n = None
    for hreq in reqlist:
        logger.info("Processing HTTP Request ID {} / {}".format(hreq[0], len(reqlist)))
        hreq_n = parse_httpreq(hreq[6], hreq[3], hreq[5], hreq[4], hreq[0], hreq[2], projname, session, user)
        if prev_hreq_n:
            prev_hreq_n.Next.add(hreq_n)
            graph.push(prev_hreq_n)
        
        graph.push(hreq_n)            
        prev_hreq_n = hreq_n

def  insert_httpresps(resplist, projname, session, user):
    logger.info("Importing {} HTTP responses and relationships with HTTP requests...".format(len(resplist)))
    # just in case, we order by ID.
    resplist = sorted(resplist, key=lambda r:r[0]) 
    
    for hres in resplist:
        logger.info("Processing HTTP Response ID {} / {}".format(hres[0], len(resplist)))
        hres_n = parse_httpres(hres[3], hres[4], hres[5], hres[0], hres[2], projname, session, user)
        
        hreq_n = HTTPRequest.select(graph).where(seq=hres[1], projname=projname, session=session, user=user).first()
        hreq_n.Transaction.add(hres_n)
        graph.push(hreq_n)
        graph.push(hres_n)

def  insert_cmd2http(idlist, projname, session, user):   
    logger.info("Importing {} relationships between Selenese commands and HTTP requests...".format(len(idlist)))
    
    for rid, cmdid in idlist:
        logger.info("Processing Selense command ID {} -> HTTP request ID {}  / {}".format(cmdid, rid, len(idlist)))
        cmd_n = SeleneseCommand.select(graph).where(seq=cmdid, projname=projname, session=session, user=user).first()
        hreq_n = HTTPRequest.select(graph).where(seq=rid, projname=projname, session=session, user=user).first()
        cmd_n.Caused.add(hreq_n)
        graph.push(cmd_n)

def insert_queries(queries, projname, session, user):
    logger.info("Importing {} SQL queries and relationships with HTTP requests...".format(len(queries)))
    
    for hreq_id, q_id, sql in queries:
        logger.info("Processing SQL query ID {} -> HTTP request ID {} / {}".format(q_id, hreq_id, len(queries)))
        n_id = "{}.{}".format(hreq_id, q_id)
        sql_n = parse_sql(sql, n_id, None, projname, session, user)
        hreq_n = HTTPRequest.select(graph).where(seq=hreq_id, projname=projname, session=session, user=user).first()
        hreq_n.Caused.add(sql_n)
        graph.push(hreq_n)
        graph.push(sql_n)

def import_all(args):
    logger.info("Importing all from vilanoo2/mosgi SQLite Database...")
    
    cmdlist = load_selcmd_sqlite(args.raw_filename)
    insert_selenese_commands(cmdlist, args.projname, args.session, args.user)
    
    hreqs = load_hreqs_sqlite(args.raw_filename)
    insert_httpreqs(hreqs, args.projname, args.session, args.user)

    hress = load_hres_sqlite(args.raw_filename)
    insert_httpresps(hress, args.projname, args.session, args.user)

    ids = load_cmd2http_sqlite(args.raw_filename)
    insert_cmd2http(ids, args.projname, args.session, args.user)

    ids = load_queries_sqlite(args.parsed_filename)
    insert_queries(ids, args.projname, args.session, args.user)

def import_selenese(args):
    cmdlist = load_selcmd_sqlite(args.filename)
    insert_selenese_commands(cmdlist, args.projname, args.session, args.user)

def import_http(args):
    hreqs = load_hreqs_sqlite(args.filename)
    insert_httpreqs(hreqs, args.projname, args.session, args.user)

    hress = load_hres_sqlite(args.filename)
    insert_httpresps(hress, args.projname, args.session, args.user)

def import_rel_selhttp(args):
    ids = load_cmd2http_sqlite(args.filename)
    insert_cmd2http(ids, args.projname, args.session, args.user)

def import_sql(args):
    ids = load_queries_sqlite(args.filename)
    insert_queries(ids, args.projname, args.session, args.user)



def parse_args(args):
    p = argparse.ArgumentParser(description='dbmanager parameters')
    subp = p.add_subparsers()

    init_p = subp.add_parser("init", help="Initialize the database")
    init_p.set_defaults(func=init_database)
    
    reset_p = subp.add_parser("reset", help="Reset the database")
    reset_p.set_defaults(func=reset_database)

    imp_p = subp.add_parser("import", help="Import data")
    imp_subp = imp_p.add_subparsers()

    imp_all_p = imp_subp.add_parser("all",    help="Import all data into Neo4j")
    imp_all_p.add_argument("raw_filename",    help="vilanoo2/mosgi SQLite3 database")
    imp_all_p.add_argument("parsed_filename", help="Analyzer SQLite3 database")    
    imp_all_p.add_argument("projname", help="Project name")
    imp_all_p.add_argument("session",  help="Session identifier")
    imp_all_p.add_argument("user",     help="User identifier")
    imp_all_p.set_defaults(func=import_all)

    imp_sel_p = imp_subp.add_parser("selenese", help="Import Selenese Commands from vilanoo2 SQLite3 database")
    imp_sel_p.add_argument("filename", help="Vilanoo2 SQLite3 database filename")
    imp_sel_p.add_argument("projname", help="Project name")
    imp_sel_p.add_argument("session",  help="Session identifier")
    imp_sel_p.add_argument("user",     help="User identifier")
    imp_sel_p.set_defaults(func=import_selenese)

    imp_sel_p = imp_subp.add_parser("http", help="Import HTTP from vilanoo2 SQLite3 database")
    imp_sel_p.add_argument("filename", help="Vilanoo2 SQLite3 database filename")
    imp_sel_p.add_argument("projname", help="Project name")
    imp_sel_p.add_argument("session",  help="Session identifier")
    imp_sel_p.add_argument("user",     help="User identifier")
    imp_sel_p.set_defaults(func=import_http)

    imp_sel_p = imp_subp.add_parser("rel_selhttp", help="Import causality relationships between Selenese command and HTTP from from vilanoo2/mosgi SQLite3 database")
    imp_sel_p.add_argument("filename", help="Vilanoo2/mosgi SQLite3 database filename")
    imp_sel_p.add_argument("projname", help="Project name")
    imp_sel_p.add_argument("session",  help="Session identifier")
    imp_sel_p.add_argument("user",     help="User identifier")
    imp_sel_p.set_defaults(func=import_rel_selhttp)

    imp_sel_p = imp_subp.add_parser("sql", help="Import SQL queries from Analyzer SQLite3 database")
    imp_sel_p.add_argument("filename", help="Analyzer SQLite3 database filename")
    imp_sel_p.add_argument("projname", help="Project name")
    imp_sel_p.add_argument("session",  help="Session identifier")
    imp_sel_p.add_argument("user",     help="User identifier")
    imp_sel_p.set_defaults(func=import_sql)

    return p.parse_args(args)

def main(args):
    global args_obj
    args_obj = parse_args(args)

    args_obj.func(args_obj)

if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
