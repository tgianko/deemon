#!/usr/bin/python2
import sys
import argparse
import sqlmodel.dataAccess as sqlDataAccess
import utils.log as log
import insertGraphData
import neo4jmodel.UserActionLevel as ual
import neo4jmodel.BrowserActionLevel as bal
import neo4jmodel.ApplicationDataLevelSQL as adlsql
from py2neo.database import Graph


NEO4J_HOST = "localhost"
NEO4J_USERNAME = "neo4j"
NEO4J_PASSWORD = "seesurf"


DEBUG = False
VERBOSITY = 1

data1 = "test"


if DEBUG:
    log.LEVEL = log.LEVELS[-1]
else:
    log.LEVEL = log.LEVELS[0]

# global vars are the devils tool
# graph = Graph(host=NEO4J_HOST, user=NEO4J_USERNAME, password=NEO4J_PASSWORD)

# argument parser object
# args_obj = None #global variables are the devils tool

# really dislike havin the setup code here!
UNIQUENESS = [ual.SeleneseCommand.__name__,
              bal.HTTPRequest.__name__,
              bal.HTTPResponse.__name__,
              adlsql.SQLQuery.__name__]

INDEX = [(ual.SeleneseCommand.__name__,
          ["projname", "seq", "session", "user"]),
         (bal.HTTPRequest.__name__,
          ["projname", "seq", "session", "user"]),
         (bal.HTTPResponse.__name__,
          ["projname", "seq", "session", "user"]),
         (adlsql.SQLQuery.__name__,
          ["projname", "seq", "session", "user"])]


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
    cmdlist = sqlDataAccess.load_selcmd_sqlite(args.filename)
    insertGraphData.insert_selenese_commands(graph, cmdlist, args.projname,
                                             args.session, args.user, logger)


def import_http(args, graph, logger=None):
    hreqs = sqlDataAccess.load_hreqs_sqlite(args.filename)
    insertGraphData.insert_httpreqs(graph, hreqs, args.projname,
                                    args.session, args.user, logger)

    hress = sqlDataAccess.load_hres_sqlite(args.filename)
    insertGraphData.insert_httpresps(graph, hress, args.projname,
                                     args.session, args.user, logger)


def import_rel_selhttp(args, graph, logger=None):
    ids = sqlDataAccess.load_cmd2http_sqlite(args.filename)
    insertGraphData.insert_cmd2http(graph, ids, args.projname,
                                    args.session, args.user, logger)


def import_sql(args, graph, logger=None):
    ids = sqlDataAccess.load_queries_sqlite(args.filename)
    insertGraphData.insert_queries(graph, ids, args.projname,
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
    imp_all_p.add_argument("raw_filename", help="vilanoo2/mosgi\
 SQLite3 database")
    imp_all_p.add_argument("parsed_filename", help="Analyzer SQLite3 database")
    imp_all_p.add_argument("projname", help="Project name")
    imp_all_p.add_argument("session",  help="Session identifier")
    imp_all_p.add_argument("user",     help="User identifier")
    imp_all_p.set_defaults(func=import_all)

    imp_sel_p = imp_subp.add_parser("selenese", help="Import Selenese Commands\
 from vilanoo2 SQLite3 database")
    imp_sel_p.add_argument("filename", help="Vilanoo2 SQLite3\
 database filename")
    imp_sel_p.add_argument("projname", help="Project name")
    imp_sel_p.add_argument("session",  help="Session identifier")
    imp_sel_p.add_argument("user",     help="User identifier")
    imp_sel_p.set_defaults(func=import_selenese)

    imp_sel_p = imp_subp.add_parser("http", help="Import HTTP from\
 vilanoo2 SQLite3 database")
    imp_sel_p.add_argument("filename", help="Vilanoo2 SQLite3\
 database filename")
    imp_sel_p.add_argument("projname", help="Project name")
    imp_sel_p.add_argument("session",  help="Session identifier")
    imp_sel_p.add_argument("user",     help="User identifier")
    imp_sel_p.set_defaults(func=import_http)

    imp_sel_p = imp_subp.add_parser("rel_selhttp", help="Import causality\
 relationships between Selenese command and HTTP from from vilanoo2/mosgi\
 SQLite3 database")
    imp_sel_p.add_argument("filename", help="Vilanoo2/mosgi SQLite3\
 database filename")
    imp_sel_p.add_argument("projname", help="Project name")
    imp_sel_p.add_argument("session",  help="Session identifier")
    imp_sel_p.add_argument("user",     help="User identifier")
    imp_sel_p.set_defaults(func=import_rel_selhttp)

    imp_sel_p = imp_subp.add_parser("sql", help="Import SQL queries\
 from Analyzer SQLite3 database")
    imp_sel_p.add_argument("filename", help="Analyzer SQLite3\
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
