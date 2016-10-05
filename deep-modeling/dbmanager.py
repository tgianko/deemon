#!/usr/bin/env python
import sys
import argparse
import utils.log as log
from api.datamodel.selenese import *
from api.datamodel.http import *
from api.datamodel.sql import *
from api.acquisition import *
from py2neo.database import Graph
from py2neo import watch
import sqlite3 as lite
from shared.config import *

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
UNIQUENESS = [SeleneseCommand.__name__,
              HTTPRequest.__name__,
              HTTPResponse.__name__,
              SQLQuery.__name__]

INDEX = [(SeleneseCommand.__name__,
          ["projname", "seq", "session", "user"]),
         (HTTPRequest.__name__,
          ["projname", "seq", "session", "user"]),
         (HTTPResponse.__name__,
          ["projname", "seq", "session", "user"]),
         (SQLQuery.__name__,
          ["projname", "seq", "session", "user"])]


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

def init_database(args, graph, logger=None):
    if logger is not None:
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


def reset_database(args, graph, logger=None):
    if logger is not None:
        logger.info("Deleting nodes and relationships. To COMPLETELY reset\
 the database please use: rm -Rf data/graph.db.\
 WARNING: this will permanently remove data of *ALL* projects")

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


def import_all(args, graph, logger=None):
    if logger is not None:
        logger.info("Importing all from vilanoo2/mosgi SQLite Database...")

    import_selenese(args, graph, logger)

    import_http(args, graph, logger)

    import_rel_selhttp(args, graph, logger)

    import_sql(args, graph, logger)


def import_selenese(args, graph, logger=None):
    cmdlist = load_selcmd_sqlite(args.raw_filename)
    insert_selenese_commands(graph, cmdlist, args.projname,
                                             args.session, args.user, logger)


def import_http(args, graph, logger=None):
    hreqs = load_hreqs_sqlite(args.raw_filename)
    insert_httpreqs(graph, hreqs, args.projname,
                                    args.session, args.user, logger)

    hress = load_hres_sqlite(args.raw_filename)
    insert_httpresps(graph, hress, args.projname,
                                     args.session, args.user, logger)


def import_rel_selhttp(args, graph, logger=None):
    ids = load_cmd2http_sqlite(args.raw_filename)
    insert_cmd2http(graph, ids, args.projname,
                                    args.session, args.user, logger)


def import_sql(args, graph, logger=None):
    ids = load_queries_sqlite(args.parsed_filename)
    insert_queries(graph, ids, args.projname,
                                   args.session, args.user, logger)


def parse_args(args):
    p = argparse.ArgumentParser(description='dbmanager parameters')
    subp = p.add_subparsers()

    init_p = subp.add_parser("init", help="Initialize the database")
    init_p.set_defaults(func=init_database)

    reset_p = subp.add_parser("reset", help="Reset the database")
    reset_p.set_defaults(func=reset_database)

    imp_p = subp.add_parser("import", help="Import data")
    imp_subp = imp_p.add_subparsers()

    imp_all_p = imp_subp.add_parser("all", help="Import all data into Neo4j")
    imp_all_p.add_argument("raw_filename", help="Vilanoo2 SQLite3\
 database filename")
    imp_all_p.add_argument("parsed_filename", help="Analyzer SQLite3 database")
    imp_all_p.add_argument("projname", help="Project name")
    imp_all_p.add_argument("session",  help="Session identifier")
    imp_all_p.add_argument("user",     help="User identifier")
    imp_all_p.set_defaults(func=import_all)

    imp_sel_p = imp_subp.add_parser("selenese", help="Import Selenese Commands\
 from vilanoo2 SQLite3 database")
    imp_sel_p.add_argument("raw_filename", help="Vilanoo2 SQLite3\
 database filename")
    imp_sel_p.add_argument("projname", help="Project name")
    imp_sel_p.add_argument("session",  help="Session identifier")
    imp_sel_p.add_argument("user",     help="User identifier")
    imp_sel_p.set_defaults(func=import_selenese)

    imp_sel_p = imp_subp.add_parser("http", help="Import HTTP from\
 vilanoo2 SQLite3 database")
    imp_sel_p.add_argument("raw_filename", help="Vilanoo2 SQLite3\
 database filename")
    imp_sel_p.add_argument("projname", help="Project name")
    imp_sel_p.add_argument("session",  help="Session identifier")
    imp_sel_p.add_argument("user",     help="User identifier")
    imp_sel_p.set_defaults(func=import_http)

    imp_sel_p = imp_subp.add_parser("rel_selhttp", help="Import causality\
 relationships between Selenese command and HTTP from from vilanoo2\
 SQLite3 database")
    imp_sel_p.add_argument("raw_filename", help="Vilanoo2 SQLite3\
 database filename")
    imp_sel_p.add_argument("projname", help="Project name")
    imp_sel_p.add_argument("session",  help="Session identifier")
    imp_sel_p.add_argument("user",     help="User identifier")
    imp_sel_p.set_defaults(func=import_rel_selhttp)

    imp_sel_p = imp_subp.add_parser("sql", help="Import SQL queries\
 from Analyzer SQLite3 database")
    imp_sel_p.add_argument("parsed_filename", help="Analyzer SQLite3\
 database filename")
    imp_sel_p.add_argument("projname", help="Project name")
    imp_sel_p.add_argument("session",  help="Session identifier")
    imp_sel_p.add_argument("user",     help="User identifier")
    imp_sel_p.set_defaults(func=import_sql)

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
