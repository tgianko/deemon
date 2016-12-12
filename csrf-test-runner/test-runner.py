#!/usr/bin/env python
import sys
import argparse
import utils.log as log
from urlparse import urlunparse, urlparse
import sqlite3 as lite
import json
import datetime

import requests
from requests.packages.urllib3.exceptions import InsecureRequestWarning
requests.packages.urllib3.disable_warnings(InsecureRequestWarning)

import socket

DEBUG = False
TIMEOUT = 120
MAX_RETRY = 3

if DEBUG:
    log.LEVEL = log.LEVELS[-1]
else:
    log.LEVEL = log.LEVELS[0]

logger = log.getdebuglogger("csrf-test-runner")

# MOSGI STUFF

# MOSGI logger
m_logger  = log.getdebuglogger("mosgi")

# MOSGI connection
mosgi_connection = None
mosgi_start_command_byte=0
mosgi_finish_response_byte=2    

def connect_to_mosgi(address, port):
    global mosgi_connection
    m_logger.info("Connecting to MOSGI: {}:{}".format(address, port))
    for i in range(0, MAX_RETRY+1):
        try:
            mosgi_connection = socket.socket(socket.AF_INET,socket.SOCK_STREAM)
            mosgi_connection.connect((address, port))
            m_logger.info("Connected to MOSGI")
            return
        except Exception as e:
            m_logger.warning("Connection to MOSGI failed (attempt {} of {}): {} {}".format(i+1, MAX_RETRY+1, type(e), e))
    m_logger.fatal("Unable to connect to MOSGI")
    



def send_start_to_mosgi(db_id):
    command = bytearray([mosgi_start_command_byte])
    logger.debug("Passing {0} to MOSGI".format(command))
    
    mosgi_connection.send(command)
    #this should explode (booooom!) the int into 4 bytes and transmit them to mosgi
    request_id = bytearray( [ ((db_id>>24) & 0xff) ,
                              ((db_id>>16) & 0xff), 
                              ((db_id>>8) & 0xff), 
                              (db_id & 0xff) ] )
    mosgi_connection.send(request_id)
    rcv = mosgi_connection.recv(1)


def fetch_requests(filename):
    logger.info("Loading testcases...")

    con = lite.connect(filename)
    reqs = []
    with con:
        cur = con.cursor()
        rs = cur.execute("SELECT * FROM CSRF_tests ORDER BY id")
        reqs = list(rs)
    return reqs

def fetch_request_by_id(filename, seq_id):
    logger.info("Loading testcases...")

    con = lite.connect(filename)
    reqs = []
    with con:
        cur = con.cursor()
        rs = cur.execute("SELECT * FROM CSRF_tests WHERE seq_id = ? ORDER BY id", (seq_id,))
        reqs = list(rs)
    return reqs

def store_httpresp(req_id, response, filename):

    headers = json.dumps(dict(response.headers))

    con = lite.connect(filename)   
    con.text_factory = str     
    with con:            
        cur = con.cursor()           
        data = (req_id, datetime.datetime.now(), response.status_code, headers, response.content)
        cur.execute("INSERT INTO http_responses (req_id, time, status_code, headers, content) VALUES(?,?,?,?,?)",
                    data)

    return 

def do_send_req(command, url, headers, body, proxy=None):
    """
    This function turns a ParseTree in an actual HTTP request. All
    values are used except the one un TN.
    """    

    proxies = {}
    if proxy:
        proxies = {
            "https": proxy,
            #"http": proxy
        }
        print proxies


    logger.info("URL {}, HEADERS {}".format(url, headers))
    sess = requests.Session()
    reqobj = requests.Request(command, url, data=body, headers=headers)
    prep_reqobj = reqobj.prepare()
    respobj = sess.send(
        prep_reqobj,
        proxies=proxies,
        timeout=TIMEOUT, 
        stream=True, 
        allow_redirects=False, 
        verify=False
    )
    sess.close()
    return reqobj, respobj
        
    
def parse_args(args):
    p = argparse.ArgumentParser(description='CSRF test runner')
    p.add_argument("-b", "--base-url",
                   dest="baseurl",                      
                   help="Base URL for the generation of testcases",       
                   default='127.0.0.1', 
                   metavar="IP",   
                   type=str)
    p.add_argument("-p", "--proxy",
                   dest="proxy",                      
                   help="HTTP Proxy, e.g., 127.0.0.1:8080",       
                   default=None, 
                   metavar="IP[:PORT]",   
                   type=str)
    p.add_argument("-M", "--mosgi-address", 
                   dest="mosgi_addr",                
                   help="MOSGI listening address.",      
                   default='127.0.0.1',            
                   metavar="IP",   
                   type=str)
    p.add_argument("-P", "--mosgi-port",
                   dest="mosgi_port",
                   help="MOSGI TCP port.",         
                   default=8844,      
                   metavar="PORT", 
                   type=int)
    p.add_argument("--no-mosgi",      
                   dest="dismosgi",                  
                   help="By default, MOSGI is enabled. Use this option to disable MOSGI.",      
                   action="store_false") 
    p.add_argument("-d", "--database",
                   dest="database",                      
                   help="Database containing test cases",
                   required=True,
                   metavar="PATH",   
                   type=str)
    p.add_argument("-r", "--run-single",
                   dest="runsingle",                      
                   help="Run single test",       
                   default=-1, 
                   metavar="INT",   
                   type=int)


    return p.parse_args(args)

def main(args):
    # global args_obj # global variables are the devils tool
    
    args_obj = parse_args(args)

    if args_obj.database:
        logger.info("Using input database {}".format(args_obj.database))

    if args_obj.dismosgi:
        connect_to_mosgi(args_obj.mosgi_addr, args_obj.mosgi_port)

    if args_obj.runsingle >= 0:
        logger.info("Running single test seq_id={}".format(args_obj.runsingle))
        requests = fetch_request_by_id(args_obj.database, args_obj.runsingle)
    else:
        requests = fetch_requests(args_obj.database)

    for test in requests:
        seq_id = test[1]
        command, url, headers, body = test[7:]
        headers = json.loads(headers)
        # Correct URL 
        if args_obj.baseurl:
            urlparse(url)
            urlp = urlparse(url)
            repl_urlp = urlp._replace(netloc=args_obj.baseurl)
            url = urlunparse(repl_urlp)
        
        logger.info("Executing test {}/{} : {}, {}".format(seq_id, len(requests), command, url))

        reqobj, respobj = do_send_req(command, url, headers, body)

        store_httpresp(seq_id, respobj, args_obj.database)

        if args_obj.dismosgi:
            send_start_to_mosgi(seq_id)

        print respobj


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))


"""
Additional indexes:
:DataValue(value)
:KeyValuePair(value)
:SQLToken(value)
"""
