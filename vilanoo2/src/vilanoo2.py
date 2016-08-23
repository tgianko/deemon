#!/usr/bin/python2.7
# -*- coding: utf-8 -*-
from proxy2.proxy2 import *
import sqlite3 as lite
import datetime
import time
import utils.log as log
import argparse
import netaddr
import socket


DEBUG     = False
VERBOSITY = 1
SIM_DELAY = False
DELAY     = 1
MOSGI     = True

if DEBUG:
    log.LEVEL = log.LEVELS[-1]
else:
    log.LEVEL = log.LEVELS[0]

# Installing two loggers
v_logger    = log.getdebuglogger("vilanoo2")
m_logger    = log.getdebuglogger("mosgi")

# MOSGI connection
mosgi_connection = None
mosgi_start_command_byte=0
mosgi_finish_response_byte=2    

# Database parameters
sqlitedb = None
sqlite_schema = os.path.join(os.getcwd(), "../../data/DBSchemaDump.sql")

# Lock to remove async requests toward upstream server
lock = threading.Lock()

# return 404 for external requests
noextreq = False


def http_to_logevt(req, res):
    return "{:5s} {:2s} {:70s} {:3s} {:10s}".format(req.command, "" if request_relevant_p(req) else "I!" , req.path, str(res.status), res.reason)

def check_and_create():
    # If the DB does not exist, lite.connect does not create a folder. 
    # Check folder first...
    dirname = os.path.dirname(sqlitedb)
    if len(dirname) > 0 and not os.path.exists(dirname):
        v_logger.info("DB {0} does not exist. Creating...".format(dirname))
        os.makedirs(dirname)

    #if not os.path.exists(sqlite_schema):
    #    v_logger.fatal("Houston, we have a problem. sqlite_schema {0} does not exist.".format(sqlite_schema))

    if not os.path.exists(sqlitedb):
        v_logger.info("DB {0} does not exist. Creating...".format(sqlitedb))
        
        f = open(sqlite_schema)
        con = lite.connect(sqlitedb)
        with con:            
            cur = con.cursor()
            with f:
                schema = f.read()
                cur.executescript(schema)


def insert_http_request(request,request_body):
    def parse_qsl(s):
        return '\n'.join("%-20s %s" % (k, v) for k, v in urlparse.parse_qsl(s, keep_blank_values=True))

    headers = ""
    for key,value in request.headers.items():
        headers = headers + key + "=" + value + ";"
    body = request_body
    cookies =""
    cookie = request.headers.get('Cookie', '')
    if cookie:
        cookies = parse_qsl(re.sub(r';\s*', '&', cookie))
            
    method_type = request.command
    check_and_create()    

    con = lite.connect(sqlitedb)        
    with con:            
        cur = con.cursor()            
        ##inserting the http_request that triggered the sql_queries            
        http_request_query_data = (datetime.datetime.now(),request.path,headers,body,method_type,cookies,"unknown")
        cur.execute("INSERT INTO http_requests (time,request_url,header,request_body,method_type,cookies,status_code) VALUES(?,?,?,?,?,?,?)",
                    http_request_query_data)
        request_id = cur.lastrowid

    request.db_request_id = lambda: None
    setattr(request,'db_request_id',request_id)

    return request_id


def update_request_status(db_request_id,status_code):
    con = lite.connect(sqlitedb)
    
    with con:            
        cur = con.cursor()
        cur.execute("UPDATE http_requests SET status_code=? WHERE id=? ORDER BY id desc LIMIT 1",(status_code,db_request_id))

def external_request(req):
    u = urlparse.urlsplit(req.path)
    ip = netaddr.IPAddress(socket.gethostbyname(u.netloc))
    if ip.is_private():
        return False

    if ip.is_loopback():
        return False

    return True

def request_relevant_p(req):
    non_relevant_extensions = [".css", ".js",".png",".jpg",".jpeg",".woff2", ".woff", ".gif", ".ico"]
    u = urlparse.urlsplit(req.path)
    
    if external_request(req):
        return False

    filename, file_extension = os.path.splitext(u.path)
    if file_extension in non_relevant_extensions:
        return False
    
    return True

class VilanooProxyRequestHandler(ProxyRequestHandler):
    Q=[]

    def do_GET(self):
        if external_request(self):
            ProxyRequestHandler.do_GET(self)
        else:
            with lock:
                ProxyRequestHandler.do_GET(self)

    def request_handler(self, req, req_body):

        req_header_text = "%s %s %s\n%s" % (req.command, req.path, req.request_version, req.headers)
        v_logger.debug(with_color(32, req_header_text))


        if MOSGI:
            m_logger.debug("===================start=========================")

        return

    def response_handler(self, req, req_body, res, res_body):
        res_header_text = "%s %d %s\n%s" % (res.response_version, res.status, res.reason, res.headers)   
        v_logger.debug(with_color(32, res_header_text))
        
        #if DEBUG:
        #    res_header_text = "%s %d %s\n%s" % (res.response_version, res.status, res.reason, res.headers)       
        #    print with_color(32, res_header_text)
        
        if request_relevant_p(req):
            v_logger.debug("Storing HTTP request and body into DB")
            db_id = insert_http_request(req,req_body)
            update_request_status(db_id,res.status)
            if MOSGI:
                command = bytearray([mosgi_start_command_byte])
                v_logger.info("Passing {0} to MOSGI".format(command))
                mosgi_connection.send(command)
                #this should explode (booooom!) the int into 4 bytes and transmit them to mosgi
                request_id = bytearray( [ ((db_id>>24) & 0xff) ,
                                          ((db_id>>16) & 0xff), 
                                          ((db_id>>8) & 0xff), 
                                          (db_id & 0xff) ] )
                mosgi_connection.send(request_id)
                rcv = mosgi_connection.recv(1)
                # To enable these two lines, please use DEBUG
                # res_header_text = "%s %d %s\n%s" % (res.response_version, res.status, res.reason, res.headers)       
                # print with_color(32, res_header_text)
                m_logger.debug("===================finished======================")
            

        
        if SIM_DELAY:
            time.sleep(DELAY)
        return

    def save_handler(self, req, req_body, res, res_body):
    	if DEBUG:
    		self.print_info(req, req_body, res, res_body)
    	else:
    		v_logger.info(http_to_logevt(req, res))

def run_proxy(address, port, HandlerClass=ProxyRequestHandler, ServerClass=ThreadingHTTPServer, protocol="HTTP/1.1"):
    server_address = (address, port)

    HandlerClass.protocol_version = protocol
    httpd = ServerClass(server_address, HandlerClass)

    sa = httpd.socket.getsockname()
    v_logger.info("Serving HTTP Proxy on {0} port {1}".format(sa[0], sa[1]))
    httpd.serve_forever()


def connect_to_mosgi(address, port):
    global mosgi_connection
    mosgi_connection = socket.socket(socket.AF_INET,socket.SOCK_STREAM)
    mosgi_connection.connect((address, port))

def main(args):
    program_name = os.path.basename(sys.argv[0])

    #try:
    parser = argparse.ArgumentParser(description='Main vilanoo2 proxy parameters')
    parser.add_argument("-b", "--bind",          dest="bind",                      help="bind address",       default='',          metavar="IP",   type=str)
    parser.add_argument("-p", "--port",          dest="port",                      help="listenig TCP port",  default='8080',      metavar="PORT", type=int)
    parser.add_argument("-M", "--mosgi-address", dest="mosgi_addr",                help="MOSGI address",      default='127.0.0.1', metavar="IP",   type=str)
    parser.add_argument("-P", "--mosgi-port",    dest="mosgi_port",                help="MOSGI port",         default='9292',      metavar="PORT", type=int)
    parser.add_argument("-s", "--sqlitedb",      dest="sqlitedb",   required=True, help="SQLite3 DB",                              metavar="PATH", type=str)
    parser.add_argument(      "--no-mosgi",      dest="dismosgi",                  help="Disable MOSGI",      action="store_false")
    parser.add_argument(      "--no-ext-req",    dest="noextreq",                  help="Return 404 for external requests", action="store_true")
    

    arg_obj = parser.parse_args(args)

    bind          = arg_obj.bind
    port          = arg_obj.port
    mosgi_address = arg_obj.mosgi_addr
    mosgi_port    = arg_obj.mosgi_port
    global sqlitedb
    sqlitedb      = arg_obj.sqlitedb
    global MOSGI
    MOSGI         = arg_obj.dismosgi
    global noextreq
    noextreq      = arg_obj.noextreq

    v_logger.info("DEBUG     enabled {0}".format(DEBUG))
    v_logger.info("MOSGI     enabled {0}".format(MOSGI))
    v_logger.info("SIM_DELAY enabled {0}".format(SIM_DELAY))
    v_logger.info("sqlitedb          {0}".format(sqlitedb))
    v_logger.info("sqlite_schema     {0}".format(sqlite_schema))
    
    check_and_create()

    if MOSGI:
        connect_to_mosgi(mosgi_address, mosgi_port)
    
    run_proxy(bind, port, HandlerClass=VilanooProxyRequestHandler)
    
    return 0
    # except KeyboardInterrupt:
    #     ### handle keyboard interrupt ###
    #     return 0
    # except Exception, e:
    #     if DEBUG:
    #         raise(e)
    #     indent = len(program_name) * " "
    #     sys.stderr.write(program_name + ": " + repr(e) + "\n")
    #     sys.stderr.write(indent + "  for help use --help\n")
    #     return 2

if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
