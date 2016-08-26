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


def init_database(args):
    logger.info("Initialize database")
    names = [SeleneseCommand.__name__,
             HTTPRequest.__name__,
             HTTPResponse.__name__]
    for name in names:
        try:
            graph.schema.create_uniqueness_constraint(name, "uuid")
        except Exception, e:
            logger.error(e)


def reset_database(args):
    logger.info("Deleting nodes and relationships. To COMPLETELY reset the database please use: rm -Rf data/graph.db. WARNING: this will permanently remove data of *ALL* projects")
    names = [SeleneseCommand.__name__,
             HTTPRequest.__name__,
             HTTPResponse.__name__]
    for name in names:
        try:
            graph.schema.drop_uniqueness_constraint(name, "uuid")
        except Exception, e:
            logger.error(e)

    graph.delete_all()
    logger.info("Done. Too late for regrets.")


def insert_selenese_commands(cmdlist, projname, session, user):
    # just in case, we order by ID.
    cmdlist = sorted(cmdlist, key=lambda cmd:cmd[0]) 

    prev_cmd_n = None
    for cmd in cmdlist:
        cmd_n = parse_selcmd(cmd[2], cmd[3], cmd[4], cmd[0], None, projname, session, user)  

        if prev_cmd_n:
            prev_cmd_n.Next.add(cmd_n)
            graph.push(prev_cmd_n)
        
        graph.push(cmd_n)        

        prev_cmd_n = cmd_n


def load_selcmd_sqlite(fname):
    con = lite.connect(fname)    
    cmdlist=[]    
    with con:            
        cur = con.cursor()            
        rs = cur.execute("SELECT * FROM selenese_commands ORDER BY id")
        cmdlist = list(rs)
    return cmdlist
    
def load_hreqs_sqlite(fname):
    con = lite.connect(fname)
    reqlist = []
    with con:            
        cur = con.cursor()            
        rs = cur.execute("SELECT * FROM http_requests ORDER BY id")
        reqlist = list(rs)
    return reqlist

def load_hres_sqlite(fname):
    con = lite.connect(fname)        
    resplist = []
    with con:            
        cur = con.cursor()
        rs = cur.execute("SELECT * FROM http_responses ORDER BY id")
        resplist = list(rs)
    return resplist

def load_cmd2http_sqlite(fname):
    con = lite.connect(fname)
    ids = []    
    with con:            
        cur = con.cursor()            
        rs = cur.execute("SELECT id, command_id FROM http_requests")
        ids = list(rs)
    return ids

def load_queries_sqlite(fname):
    con = lite.connect(fname)
    ids = []    
    with con:            
        cur = con.cursor()            
        rs = cur.execute("SELECT * FROM sql_queries")
        ids = list(rs)
    return ids

def  insert_httpreqs(reqlist, projname, session, user):
    # just in case, we order by ID.
    reqlist = sorted(reqlist, key=lambda r:r[4]) 
    
    prev_hreq_n = None
    for hreq in reqlist:
        hreq_n = parse_httpreq(hreq[6], hreq[3], hreq[5], hreq[4], hreq[0], hreq[2], projname, session, user)
        if prev_hreq_n:
            prev_hreq_n.Next.add(hreq_n)
            graph.push(prev_hreq_n)
        
        graph.push(hreq_n)            
        prev_hreq_n = hreq_n

def  insert_httpresps(resplist, projname, session, user):
    # just in case, we order by ID.
    resplist = sorted(resplist, key=lambda r:r[0]) 
    
    for hres in resplist:
        hres_n = parse_httpres(hres[3], hres[4], hres[5], hres[0], hres[2], projname, session, user)
        
        hreq_n = HTTPRequest.select(graph).where(seq=hres[1], projname=projname, session=session, user=user).first()
        hreq_n.Transaction.add(hres_n)
        graph.push(hreq_n)
        graph.push(hres_n)

def  insert_cmd2http(idlist, projname, session, user):   

    for rid, cmdid in idlist:
        cmd_n = SeleneseCommand.select(graph).where(seq=cmdid, projname=projname, session=session, user=user).first()
        hreq_n = HTTPRequest.select(graph).where(seq=rid, projname=projname, session=session, user=user).first()
        cmd_n.Caused.add(hreq_n)
        graph.push(cmd_n)

def insert_queries(queries, projname, session, user):
    
    for hreq_id, q_id, sql in queries:
        sql_n = SQLQuery(projname, session, user, sql)

def import_sqlite(args):
    logger.info("Importing SQLite DB {} into {}".format(args.filename, args.projname))
    
    logger.info("Loading Selense commands from SQLite...")
    cmdlist = load_selcmd_sqlite(args.filename)
    logger.info("Importing...")
    insert_selenese_commands(cmdlist, args.projname, args.session, args.user)

    logger.info("Loading HTTP requests from SQLite...")
    hreqs = load_hreqs_sqlite(args.filename)
    logger.info("Importing...")
    insert_httpreqs(hreqs, args.projname, args.session, args.user)

    logger.info("Loading HTTP responses from SQLite...")
    hress = load_hres_sqlite(args.filename)
    logger.info("Importing...")
    insert_httpresps(hress, args.projname, args.session, args.user)

    logger.info("Loading Selenese command to HTTP requests relationships from SQLite...")
    ids = load_cmd2http_sqlite(args.filename)
    logger.info("Importing...") 
    insert_cmd2http(ids, args.projname, args.session, args.user)

    logger.info("Loading SQL queries from SQLite...")
    ids = load_queries_sqlite(args.filename)

def parse_args(args):
    p = argparse.ArgumentParser(description='dbmanager parameters')
    subp = p.add_subparsers()

    init_p = subp.add_parser("init", help="Initialize the database")
    init_p.set_defaults(func=init_database)
    
    reset_p = subp.add_parser("reset", help="Reset the database")
    reset_p.set_defaults(func=reset_database)

    imp_p = subp.add_parser("import", help="Import data")
    imp_subp = imp_p.add_subparsers()

    ins_sel_ts_p = imp_subp.add_parser("sqlite", help="Import SQLite3 database")
    ins_sel_ts_p.add_argument("filename", help="SQLite3 file as created by vilanoo2/mosgi")
    ins_sel_ts_p.add_argument("projname", help="Project name")
    ins_sel_ts_p.add_argument("session",  help="Session identifier")
    ins_sel_ts_p.add_argument("user",     help="User identifier")
    ins_sel_ts_p.set_defaults(func=import_sqlite)

    return p.parse_args(args)

def main(args):
    global args_obj
    args_obj = parse_args(args)

    args_obj.func(args_obj)

if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
