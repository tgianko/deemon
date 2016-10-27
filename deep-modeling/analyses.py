#!/usr/bin/python2
import sys
import argparse
import utils.log as log
from py2neo.database import Graph
from py2neo import watch
from shared.config import *
<<<<<<< HEAD
from api.datamodel.selenese import *
=======
>>>>>>> 7fa9b299547337c71252e0a0976718ce9dd305b2

if DEBUG:
    log.LEVEL = log.LEVELS[-1]
    watch("neo4j.bolt")
else:
    log.LEVEL = log.LEVELS[0]

# Installing two loggers
logger = log.getdebuglogger("analyses")


def selenese(args, graph):
    """
    Query: 
    - find all selenese commands with (projname, session, user) with a given path property X
    - for each selenese commands above, return the longest path of selenese commands ending with them
    """

    #res = graph.data("""MATCH (init:SeleneseCommand {seq:1}), (end:SeleneseCommand)-[:CAUSES]->(h:HTTPRequest {method:"POST"}), p=(init)-[:NEXT*]->(end) RETURN p, h""")
    init = SeleneseCommand.select(graph).where(seq=0)
    


def clustify(args, graph):
    """
    TODO: Include clastification here
    """
    pass

def parse_args(args):
    p = argparse.ArgumentParser(description='Deep model analyses')
    subp = p.add_subparsers()

    sel_p = subp.add_parser("tc", help="Retrieve a simple Selenese test case from the database")
    sel_p.add_argument("projname", help="Project name")
    sel_p.add_argument("session",  help="Session identifier")
    sel_p.add_argument("user",     help="User identifier")
    sel_p.set_defaults(func=selenese)

    sel_p = subp.add_parser("init", help="Run the SQL and HTTP request clustering algorithm.")
    sel_p.set_defaults(func=clustify)

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
