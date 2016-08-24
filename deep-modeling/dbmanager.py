#!/usr/bin/python2
import sys
import argparse
import utils.log as log
import utils.selenese as selenese
import sqlite3 as lite

from neo4j.v1 import GraphDatabase, basic_auth

NEO4J_USERNAME = "neo4j"
NEO4J_PASSWORD = "seesurf"

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
    driver = GraphDatabase.driver("bolt://localhost", auth=basic_auth(NEO4J_USERNAME, NEO4J_PASSWORD))
    session = driver.session()
    q = "CREATE CONSTRAINT ON (cmd:SeleneseCommand) ASSERT cmd.id IS UNIQUE"
    logger.info("Creating unique constraint onf SeleneseCommand {}".format(q))
    session.run(q);
    session.close()


def reset_database(args):
    logger.info("Deleting nodes and relationships. To COMPLETELY reset the database please use: rm -Rf data/graph.db. WARNING: this will permanently remove data of *ALL* projects")
    
    driver = GraphDatabase.driver("bolt://localhost", auth=basic_auth(NEO4J_USERNAME, NEO4J_PASSWORD))
    session = driver.session()
    q= "MATCH (n)-[r]->(m) DELETE n,r,m"
    logger.info("Creating unique constraint onf SeleneseCommand {}".format(q))
    session.run(q);
    session.close()
    logger.info("Done. Too late for regrets.")


def create_project(args):
    logger.info("Create project {}".format(args.projname))


def insert_selenese_commands(cmdlist, projname):
    # just in case, we order by ID.
    cmdlist = sorted(cmdlist, key=lambda cmd:cmd[0]) 
    
    driver = GraphDatabase.driver("bolt://localhost", auth=basic_auth(NEO4J_USERNAME, NEO4J_PASSWORD))
    session = driver.session()
    
    pos      = 0
    prev_pos = -1
    prev_cmd = None 
    prev_id  = None
    for cmd in cmdlist:
        pos      = int(cmd[0])
        filename = cmd[1]
        id       = "{}_{}_{}".format(pos,      projname, filename)
        
        data =  {"pos"     : pos, 
                "tc"       : filename, 
                "proj"     : projname, 
                "c"        : cmd[2], 
                "t"        : cmd[3], 
                "v"        : cmd[4],
                "id"       : id,
                "prev_pos" : prev_pos,
                "prev_id"  : prev_id}

        q = """CREATE (cmd:SeleneseCommand {pos:{pos}, tc:{tc}, id:{id}}), (c:String {value:{c}}), (t:String {value:{t}}), (v:String {value:{v}}),
            (cmd)-[:SeleneseCommandName]->(c),(cmd)-[:SeleneseCommandTarget]->(t),(cmd)-[:SeleneseCommandValue]->(v)"""
        
        logger.info("Adding command id:{} , ({}, {}, {})".format(data["id"], data["c"], data["t"], data["v"]))
        for e in session.run(q, parameters=data):
            print e

        if prev_cmd is not None:
            q= """MATCH (cmd1:SeleneseCommand {id:{prev_id}}), (cmd2:SeleneseCommand {id:{id}})
                CREATE (cmd1)-[:SeleneseNextCommand]->(cmd2)"""
            logger.info("       cmd {} -> {}".format(data["prev_id"], data["id"]))
            for e in session.run(q, parameters=data):
                print e

        prev_cmd = cmd
        prev_pos = pos
        prev_id  = id

    session.close()


def insert_selenese_sqlite(fname, projname):
    logger.info("        - Selenese commands")
    con = lite.connect(fname)        
    with con:            
        cur = con.cursor()            
        rs = cur.execute("SELECT * FROM selenese_commands ORDER BY id")
        cmdlist = list(rs)
        insert_selenese_commands(cmdlist, projname)

def  insert_httpreqs(reqlist, projname):
    # just in case, we order by ID.
    reqlist = sorted(reqlist, key=lambda r:r[0]) 
    
    driver = GraphDatabase.driver("bolt://localhost", auth=basic_auth(NEO4J_USERNAME, NEO4J_PASSWORD))
    session = driver.session()
    
    for r in reqlist:
        pass

    session.close()

def insert_httpconv_sqlite(fname, projname):
    logger.info("        - HTTP conversation")
    con = lite.connect(fname)        
    with con:            
        cur = con.cursor()            
        rs = cur.execute("SELECT * FROM selenese_commands ORDER BY id")
        reqlist = list(rs)
        reqlist = map(lambda r: (r[0], r[2], r[3], r[4], r[5], r[6]), reqlist)
        insert_httpreqs(reqlist, projname)


def import_sqlite(args):
    logger.info("Importing SQLite DB {} into {}".format(args.filename, args.projname))
    insert_selenese_sqlite(args.filename, args.projname)
    insert_httpconv_sqlite(args.filename, args.projname)


def parse_args(args):
    p = argparse.ArgumentParser(description='dbmanager parameters')
    subp = p.add_subparsers()

    init_p = subp.add_parser("init", help="Initialize the database")
    init_p.set_defaults(func=init_database)
    
    reset_p = subp.add_parser("reset", help="Reset the database")
    reset_p.set_defaults(func=reset_database)

    create_p = subp.add_parser("create", help="Create a new project")
    create_p.add_argument("projname", help="Create a new project")
    create_p.set_defaults(func=create_project)

    ins_sel_ts_p = subp.add_parser("import_sqlite", help="Import SQLite3 database")
    ins_sel_ts_p.add_argument("filename", help="SQLite3 file as created by vilanoo2/mosgi")
    ins_sel_ts_p.add_argument("projname", help="Project name")
    ins_sel_ts_p.set_defaults(func=import_sqlite)

    return p.parse_args(args)

def main(args):
    global args_obj
    args_obj = parse_args(args)

    args_obj.func(args_obj)

if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
