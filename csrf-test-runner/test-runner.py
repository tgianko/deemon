#!/usr/bin/env python
import sys
import argparse
import sqlite3 as lite
import json
import datetime
import time
import socket
import subprocess
import threading
import Cookie
from urlparse import urlparse, urlunparse
import re
import requests
from requests.packages.urllib3.exceptions import InsecureRequestWarning
requests.packages.urllib3.disable_warnings(InsecureRequestWarning)
#from urlparse import urlunparse, urlparse

import utils.log as log


DEBUG = False
TIMEOUT = 120
MAX_RETRY = 3

if DEBUG:
    log.LEVEL = log.LEVELS[-1]
else:
    log.LEVEL = log.LEVELS[0]

logger = log.getdebuglogger("csrf-test-runner")

# Selenese runner 
s_logger   = log.getdebuglogger("selenese")
selrun_thr = None

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
    for i in range(0, MAX_RETRY):
        try:
            mosgi_connection = socket.socket(socket.AF_INET,socket.SOCK_STREAM)
            mosgi_connection.connect((address, port))
            m_logger.info("Connected to MOSGI")
            return
        except Exception as e:
            m_logger.warning("Connection to MOSGI failed (attempt {} of {}): {} {}".format(i+1, MAX_RETRY+1, type(e), e))
            if i == MAX_RETRY - 1:
                m_logger.fatal("Unable to connect to MOSGI")
                raise e

            time.sleep(1)


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

def do_send_req(command, url, headers, body, c_obj=None, proxy=None):
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


    if c_obj and "cookie" in headers:
        if args_obj.replcookie:
            logger.info("Replacing test case cookies with fresh ones")
            headers["cookie"]= _inline_cookie(c_obj)
        else:
            logger.info("Updating test case cookies with fresh ones")
            old_c_obj = Cookie.SimpleCookie()
            old_c_obj.load(str(headers["cookie"]))
            old_c_obj.update(c_obj)
            headers["cookie"]= _inline_cookie(old_c_obj)


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

class SeleneseRunnerThread(threading.Thread):

    def run(self):
        self.stdout = []
        cmdline = ["java", 
                    "-jar",
                    "../selenese-runner/selenese-runner.jar",
                "--driver", "firefox", 
                    "--no-proxy","*.com,*.net,*.org", 
                    "-t", "640000",
                    "-i",
                    "--baseurl", args_obj.baseurl,
                    "{}".format(args_obj.selenese)]
        if args_obj.selenese_args:
            for p in args_obj.selenese_args.split(" "):
                cmdline.insert(-1, p) # w/ -1 inserts at the last but one position

        s_logger.info(cmdline)

        proc = subprocess.Popen(cmdline, bufsize=0, stdin=subprocess.PIPE, stderr=subprocess.PIPE, stdout=subprocess.PIPE)
        time.sleep(0.5)

        with open(args_obj.selenese_log, "w") as f:
            """
            Read stdout
            """            
            s_logger.info("Start running the show")
            for line in iter(proc.stdout.readline, b""):
                self.stdout.append(line) # create a copy
                f.write(line)

                if proc.poll() is not None:
                    break

                if ">>> Press ENTER to continue <<<" in line:
                    """
                    Next command
                    """
                    # Let's sleep a bit to flush pending HTTPr requests
                    s_logger.info("Selenese ready for next command. Waiting for {}s...".format(args_obj.wait))
                    time.sleep(args_obj.wait)

                    # Resume Selenese runner
                    s_logger.info("Pressing ENTER")
                    proc.stdin.write("\n")
                    s_logger.info("Pressed  ENTER")
        
        time.sleep(args_obj.wait)

        if proc.returncode != "0":
            s_logger.warning("Selenese-runner-java terminated unexpectedly with code {}. Sending SIGTERM.".format(proc.returncode))
        else:
            s_logger.info("Selenese-runner-jar terminated with code {}. Sending SIGTERM.".format(proc.returncode))
     
def parse_args(args):
    p = argparse.ArgumentParser(description='CSRF test runner')
    p.add_argument("-b", "--base-url",
                   dest="baseurl",                      
                   help="Base URL for the generation of testcases",       
                   default='127.0.0.1', 
                   metavar="IP",   
                   type=str)
    
    p.add_argument("-t", "--test_id",
                   dest="test_id",
                   required=True,                   
                   help="ID of the test to run",
                   metavar="INT",   
                   type=int)

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
    
    p.add_argument("--replace-cookie",      
                   dest="replcookie",                  
                   help="Replace test cases cookies with freshes. When not used, test cases cookies are updated.",
                   action="store_true") 

    p.add_argument("-d", "--database",
                   dest="database",                      
                   help="Database containing test cases",
                   required=True,
                   metavar="PATH",   
                   type=str)
    
    p.add_argument("-S", "--selenese",      
                   dest="selenese",
                   required=True,         
                   help="Specify the Selenese test case to login",            
                   metavar="PATH", type=str)  

    p.add_argument("-l", "--selenese-log",
                   dest="selenese_log",
                   help="the file which is the selense log for the current run",
                   metavar="PATH", type=str)

    p.add_argument(      "--selenese-args", 
                   dest="selenese_args",             
                   help="Use this parameter to pass additional CLI arguments to selenese-runner-java",            
                   metavar="ARGS", 
                   type=str)  

    p.add_argument("-w", "--wait",          
                    dest="wait",                      
                    help="Waiting time in seconds before the next Selenese command is executed.",  
                    default='2',    
                    metavar="SEC", 
                    type=float)

    return p.parse_args(args)

def _parse_cookie(c):
    pattern = """(.*)=\[(.*)\] \(domain=(.*), path=(.*), expire=(.*)\)"""
    result = re.match(pattern, c)
    key = result.group(1)
    value = result.group(2)
    domain = result.group(3)
    path = result.group(4)
    expires = result.group(5)
    return key, value, domain, path, expires

def _inline_cookie(cookie):
    """Return an inline cookie string"""
    result = []
    items = cookie.items()
    items.sort()
    for K,V in items:
        result.append( V.OutputString() )
    return "; ".join(result)

def _selout_to_cookie(buf):
    aux = [line.split("Cookie: ")[1] for line in buf if "Cookie" in line]
    
    cookie = Cookie.SimpleCookie()
    for line in aux:
        if line[0:5] in ["[add]", "[del]", "[mod]"]:
            key, value, domain, path, expires = _parse_cookie(line[5:])
        else:
            key, value, domain, path, expires = _parse_cookie(line)

        if line[0:5] == "[del]":
            del cookie[key]
        else: # line[0:5] in ["[add]", "[mod]"] or line does not start with none of them
            cookie[key] = value
            cookie[key]["domain"] = domain
            cookie[key]["path"] = path
            if expires != "*":
                cookie[key]["expires"] = expires

    return cookie
    
def login_and_get_cookie():
    logger.info("Running selenese-runner.jar")
    selrun_thr = SeleneseRunnerThread() # install a global thread
    selrun_thr.start() 

    logger.info("Waiting selenese-runner to be done")
    selrun_thr.join()

    logger.info("selenese-runner.jar has finished. No. of lines {}".format(len(selrun_thr.stdout)))
    logger.info("Parsing cookies...")
    c_obj = _selout_to_cookie(selrun_thr.stdout)
    logger.info("Fresh cookies: {}".format(c_obj.output(header="Cookie: ")))
    return c_obj

def main(args):
    global args_obj # global variables are the devils tool
    args_obj = parse_args(args)

    test = fetch_request_by_id(args_obj.database, str(args_obj.test_id))
    if len(test) == 0:
        raise Exception("No test cases in DB")

    test = test[0]
    seq_id = test[1]
    command, url, headers, body = test[11:]

    logger.info("Test case test {}: {}, {}".format(seq_id, command, url))
    
    logger.info("Retrieving fresh cookies")
    c_obj = login_and_get_cookie()

    # MOSGI needs to be run AFTER login.
    if args_obj.dismosgi:
        connect_to_mosgi(args_obj.mosgi_addr, args_obj.mosgi_port)

    headers = json.loads(headers)
    # Correct URL 
    if args_obj.baseurl:
        baseurl_p = urlparse(args_obj.baseurl)
        urlp = urlparse(url)
        repl_urlp = urlp._replace(netloc=baseurl_p.netloc)
        url = urlunparse(repl_urlp)
    
    logger.info("Sending HTTP request")
    reqobj, respobj = do_send_req(command, url, headers, body, c_obj=c_obj)
    logger.info("Received {}".format(respobj.status_code))

    logger.info("Storing HTTP response")
    store_httpresp(seq_id, respobj, args_obj.database)

    if args_obj.dismosgi:
        logger.info("Waiting for MOSGI")
        send_start_to_mosgi(seq_id)

    logger.info("Done")


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))


"""
Additional indexes:
:DataValue(value)
:KeyValuePair(value)
:SQLToken(value)
"""
