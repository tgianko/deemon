#!/usr/bin/python2
import sys
import argparse
import utils.log as log
import utils.selenese as selenese

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
    logger.info("To reset the database please use: rm -Rf data/graph.db. WARNING: this will permanently remove *ALL* data of *ALL* projects")

def create_project(args):
    logger.info("Create project {}".format(args.projname))

def insert_selenese_tc(args):
    logger.info("Insert Selenese Test Case {} into {}".format(args.filename, args.projname))
    testcase = selenese.TestCase(args.filename)

    driver = GraphDatabase.driver("bolt://localhost", auth=basic_auth(NEO4J_USERNAME, NEO4J_PASSWORD))
    session = driver.session()
    prev_cmd = None
    pos = 0
    for cmd in testcase:
        data =  {"pos"    : pos, 
                "tc"       :args.filename, 
                "proj"     :args.projname, 
                "c"        :cmd.command(), 
                "t"        :cmd.target(), 
                "v"        :cmd.value(),
                "id"       :"{}_{}_{}".format(pos, args.projname, args.filename),
                "prev_pos" :pos-1,
                "prev_id"  :"{}_{}_{}".format(pos-1, args.projname, args.filename)}

        q = """CREATE (cmd:SeleneseCommand {pos:{pos}, tc:{tc}, id:{id}}), (c:String {value:{c}}), (t:String {value:{t}}), (v:String {value:{v}}),
            (cmd)-[:SeleneseCommandName]->(c),(cmd)-[:SeleneseCommandTarget]->(t),(cmd)-[:SeleneseCommandValue]->(v)"""
        logger.info("Adding command id:{} , ({}, {}, {})".format(data["id"], data["c"], data["t"], data["v"]))
        for e in session.run(q, parameters=data):
            print e

        if prev_cmd is not None:
            
            q= """MATCH (cmd1:SeleneseCommand {id:{id}}), (cmd2:SeleneseCommand {id:{prev_id}})
                CREATE (cmd1)-[:SeleneseNextCommand]->(cmd2)"""
            logger.info("       cmd {} -> {}".format(data["prev_id"], data["id"]))
            for e in session.run(q, parameters=data):
                print e
        #for curr,next in zip (tc[0:-1], tc[1:]):
        prev_cmd = cmd
        pos+=1

def insert_selenese_ts(args):
    logger.info("Insert Selenese Test Suite {} into {}".format(args.filename, args.projname))

def insert_http_conversation(args):
    logger.info("Insert HTTP Conversation from {} into {}".format(args.filename, args.projname))



def main(args):
    p = argparse.ArgumentParser(description='dbmanager parameters')
    subp = p.add_subparsers()

    init_p = subp.add_parser("init", help="Initialize the database")
    init_p.set_defaults(func=init_database)
    
    reset_p = subp.add_parser("reset", help="Reset the database")
    reset_p.set_defaults(func=reset_database)

    create_p = subp.add_parser("create", help="Create a new project")
    create_p.add_argument("projname", help="Create a new project")
    create_p.set_defaults(func=create_project)

    ins_sel_tc_p = subp.add_parser("ins_selenese_tc", help="Insert a Selenese HTML Test Case file into a project")
    ins_sel_tc_p.add_argument("filename", help="Selenese HTML Test Case file")
    ins_sel_tc_p.add_argument("projname", help="Project name")
    ins_sel_tc_p.set_defaults(func=insert_selenese_tc)

    ins_sel_ts_p = subp.add_parser("ins_selenese_ts", help="Insert a Selenese HTML Test Suite file into a project")
    ins_sel_ts_p.add_argument("filename", help="Selenese HTML Test Suite file")
    ins_sel_ts_p.add_argument("projname", help="Project name")
    ins_sel_ts_p.set_defaults(func=insert_selenese_ts)

    ins_sel_ts_p = subp.add_parser("ins_http", help="Insert an HTTP conversation from SQLite3 file into a project")
    ins_sel_ts_p.add_argument("filename", help="SQLite3 file as created by Vilanoo2")
    ins_sel_ts_p.add_argument("projname", help="Project name")
    ins_sel_ts_p.set_defaults(func=insert_http_conversation)

    global args_obj
    args_obj = p.parse_args(args)

    args_obj.func(args_obj)

if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
