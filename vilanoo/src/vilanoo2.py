#!/usr/bin/env python
# This file is part of Deemon.

# Deemon is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# Deemon is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with Deemon.  If not, see <http://www.gnu.org/licenses/>.
# -*- coding: utf-8 -*-
from proxy2.proxy2req import *
import os  # COMMENT: Added during cleanup
import re  # COMMENT: Added during cleanup
import urlparse  # COMMENT: Added during cleanup
import sys  # COMMENT: Added during cleanup
import sqlite3 as lite
import datetime
import time
import utils.log as log
import utils.selenese as selenese
import argparse
import netaddr
import socket
import subprocess
import threading
import signal

DEBUG = False
VERBOSITY = 2
SIM_DELAY = False
DELAY = 1

# COMMENT: Parsed arguments object
args_obj = None
if DEBUG:
    log.LEVEL = log.LEVELS[-1]
else:
    log.LEVEL = log.LEVELS[0]

# COMMENT: Installing two loggers
s_logger = log.getdebuglogger("selenese-runner")
v_logger = log.getdebuglogger("vilanoo2")
m_logger = log.getdebuglogger("mosgi")

# COMMENT: MOSGI connection
mosgi_connection = None
mosgi_start_command_byte = 0
mosgi_finish_response_byte = 2

# COMMENT: Database parameters
sqlitedb = None
sqlite_schema = os.path.join(os.getcwd(), "../../data/DBSchemaDump.sql")


# COMMENT: Lock to remove async requests toward upstream server
lock = threading.Lock()


# COMMENT: Threads
selrun_thr = None

# COMMENT: Selenese command
sel_cmd_id = -1


def http_to_logevt(req, res):
    return "{:3s} {:5s} {:2s} {:70s} {:3s} {:10s}".format(str(sel_cmd_id),
                                                          req.command,
                                                          "" if request_relevant_p(req) else "I!" ,
                                                          req.path, str(res.status),
                                                          res.reason)


def sqlitedb_init():
    # COMMENT: If the DB does not exist, lite.connect does not create a folder.
    # COMMENT: Check folder first.
    dirname = os.path.dirname(args_obj.sqlitedb)
    if len(dirname) > 0 and not os.path.exists(dirname):
        v_logger.info("Folder {0} does not exist. Creating...".format(dirname))
        os.makedirs(dirname)

    if not os.path.exists(args_obj.sqlitedb):
        v_logger.info("SQLite DB file {0} does not exist. Creating from {1}".format(args_obj.sqlitedb, sqlite_schema))

        f = open(sqlite_schema)
        con = lite.connect(args_obj.sqlitedb)
        with con:
            cur = con.cursor()
            with f:
                schema = f.read()
                cur.executescript(schema)
        v_logger.info("SQLite DB file {0} created.".format(args_obj.sqlitedb))


def store_sel_commands(filename):
    tcs = []

    if selenese.is_suite(filename):
        ts = selenese.SeleneseTestSuite(filename)

        tcs = [tc for tc in ts]
    else:
        tcs = [selenese.SeleneseTestCase(filename)]

    con = lite.connect(args_obj.sqlitedb)
    with con:
        cur = con.cursor()
        # COMMENT: inserting the http_request that triggered the sql_queries
        i = 0
        for tc in tcs:
            for cmd in tc:
                data = (i, tc.name(), cmd.command(), cmd.target(), cmd.value())
                s_logger.info("Storing {:3s} {:15s} {:13s} {:13s} {:13s}".format(str(i), tc.name(), cmd.command(), cmd.target(), cmd.value()))
                cur.execute("INSERT INTO selenese_commands (id,tcname,command,target,value) VALUES(?,?,?,?,?)", data)
                i += 1


def store_httpreq(request, request_body):
    def parse_qsl(s):
        return '\n'.join("%-20s %s" % (k, v) for k, v in urlparse.parse_qsl(s, keep_blank_values=True))

    headers = "\r\n".join(["{}: {}".format(k,v) for k,v in request.headers.items()])

    body = request_body
    cookies = ""
    cookie = request.headers.get('Cookie', '')
    if cookie:
        cookies = parse_qsl(re.sub(r';\s*', '&', cookie))

    method_type = request.command

    con = lite.connect(args_obj.sqlitedb)
    con.text_factory = str
    with con:
        cur = con.cursor()
        # COMMENT: inserting the http_request that triggered the sql_queries
        data = (sel_cmd_id, datetime.datetime.now(), request.path,
                headers, body, method_type, cookies, "unknown")
        cur.execute("INSERT INTO http_requests (command_id, time, request_url, headers, request_body, method_type, cookies, status_code) VALUES(?,?,?,?,?,?,?,?)",
                    data)
        request_id = cur.lastrowid

    request.db_request_id = lambda: None
    setattr(request, 'db_request_id', request_id)

    return request_id


def store_httpresp(req_id, response, body):
    headers = "\r\n".join(["{}: {}".format(k, v) for k, v in response.headers.items()])
    con = lite.connect(args_obj.sqlitedb)
    con.text_factory = str
    with con:
        cur = con.cursor()
        data = (req_id, datetime.datetime.now(),
                response.status, headers, body)
        cur.execute("INSERT INTO http_responses (req_id, time, status_code, headers, content) VALUES(?,?,?,?,?)",
                    data)

    return


def update_httpreq_status(db_request_id, status_code):
    con = lite.connect(args_obj.sqlitedb)
    
    with con:
        cur = con.cursor()
        cur.execute("UPDATE http_requests SET status_code=? WHERE id=? ORDER BY id desc LIMIT 1",(status_code,db_request_id))


def external_request(req):
    # COMMENT: We adjust the path because when SSL is used, we get path as relative.
    path = req.path
    if path[0] == '/':
        path = "whatever://%s%s" % (req.headers['Host'], path)

    u = urlparse.urlsplit(path)
    ip = netaddr.IPAddress(socket.gethostbyname(u.netloc))
    if ip.is_private():
        return False

    if ip.is_loopback():
        return False

    return True


def request_relevant_p(req):
    non_relevant_extensions = [".css", ".js", ".png", ".jpg", ".jpeg", ".woff2", ".woff", ".gif", ".ico"]
    u = urlparse.urlsplit(req.path)

    if external_request(req):
        return False

    filename, file_extension = os.path.splitext(u.path)
    if file_extension in non_relevant_extensions:
        return False

    return True


class VilanooProxyRequestHandler(BaseHTTPRequestHandler, ProxyRequestHandler):

    timeout = 120

    def do_GET(self):
        self.close_connection = 0  # COMMENT: This line of code avoids timeoutes. Do not remove.
        if external_request(self):
            ProxyRequestHandler.do_GET(self)
        else:
            v_logger.debug("Waiting for lock {}".format(self.path))
            with lock:
                ProxyRequestHandler.do_GET(self)

    do_HEAD = do_GET
    do_POST = do_GET
    do_OPTIONS = do_GET

    def request_handler(self, req, req_body):
        req_header_text = "%s %s %s\n%s" % (req.command, req.path, req.request_version, req.headers)

        # COMMENT: HTTPS connections toward external server passes from do_GET which fails if BR is used.
        if "br" in req.headers.get("accept-encoding", ""):
            req.headers["accept-encoding"] = ",".join(filter(lambda enc: enc.strip() != "br", req.headers["accept-encoding"].split(",")))

        if VERBOSITY > 2:
            v_logger.debug(with_color(32, req_header_text))

        if args_obj.dismosgi:
            m_logger.debug("===================start=========================")

        return

    def response_handler(self, req, req_body, res, res_body):
        res_header_text = "%s %d %s\n%s" % (res.response_version, res.status, res.reason, res.headers)   
        if VERBOSITY > 2:
            v_logger.debug(with_color(32, res_header_text))

        if request_relevant_p(req):

            v_logger.debug("Storing HTTP request and responses into DB")
            db_id = store_httpreq(req, req_body)
            update_httpreq_status(db_id, res.status)
            store_httpresp(db_id, res, res_body)

            if args_obj.dismosgi:
                command = bytearray([mosgi_start_command_byte])
                v_logger.debug("Passing {0} to MOSGI".format(command))

                mosgi_connection.send(command)
                # COMMENT: this explodes the int into 4 bytes for transmission to mosgi
                request_id = bytearray([((db_id >> 24) & 0xff),
                                        ((db_id >> 16) & 0xff),
                                        ((db_id >> 8) & 0xff),
                                        (db_id & 0xff)])
                mosgi_connection.send(request_id)
                rcv = mosgi_connection.recv(1)
                m_logger.debug("===================finished======================")

        if SIM_DELAY:
            time.sleep(DELAY)
        return

    def save_handler(self, req, req_body, res, res_body):
        if DEBUG:
            if VERBOSITY >= 2:
                self.print_info(req, req_body, res, res_body)
            elif VERBOSITY > 1 and request_relevant_p(req):
                self.print_info(req, req_body, res, res_body)
            else:
                v_logger.info(http_to_logevt(req, res))
        else:
            v_logger.info(http_to_logevt(req, res))

    def log_message(self, format, *args):
        v_logger.info("{} {}".format(format, args))


def start_proxy(address, port, HandlerClass=VilanooProxyRequestHandler, ServerClass=ThreadingHTTPServer, protocol="HTTP/1.1"):
    HandlerClass.protocol_version = protocol
    httpd = ServerClass((address, port), HandlerClass)

    sa = httpd.socket.getsockname()
    v_logger.info("Serving HTTP Proxy on {0} port {1}".format(sa[0], sa[1]))
    httpd.serve_forever()
    

def connect_to_mosgi(address, port):
    m_logger.info("Connecting to MOSGI: {}:{}".format(address, port))
    global mosgi_connection
    mosgi_connection = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    mosgi_connection.connect((address, port))
    m_logger.info("Connected to MOSGI")


def start_selenese_runner(fname, selenese_log):

    def _run():
        cmdline = ["java",
                   "-jar",
                   "../../selenese-runner/selenese-runner.jar",
                   "--driver", "firefox",
                   "--proxy", "{}:{}".format(args_obj.bind, args_obj.port),
                   "--no-proxy", "*.com,*.net,*.org",
                   "-t", "640000",
                   "-i",
                   "{}".format(fname)]
        if args_obj.selenese_args:
            for p in args_obj.selenese_args.split(" "):
                cmdline.insert(-1, p)

        s_logger.info(cmdline)

        proc = subprocess.Popen(cmdline, bufsize=0, stdin=subprocess.PIPE, stderr=subprocess.PIPE, stdout=subprocess.PIPE)
        time.sleep(0.5)

        with open(selenese_log, "w") as f:
            """
            Read stdout
            """
            s_logger.info("Start running the show")
            for line in iter(proc.stdout.readline, b""):
                f.write(line)

                if proc.poll() is not None:
                    break

                if ">>> Press ENTER to continue <<<" in line:
                    """
                    Next command
                    """
                    # COMMENT: sleep to flush pending HTTP requests
                    s_logger.info("Selenese ready for next command. Waiting for {}s...".format(args_obj.wait))
                    time.sleep(args_obj.wait)

                    # COMMENT: Next ID
                    global sel_cmd_id
                    sel_cmd_id += 1

                    # COMMENT: Resume Selenese runner
                    s_logger.debug("Pressing ENTER")
                    proc.stdin.write("\n")
                    s_logger.debug("Pressed  ENTER")

        if proc.returncode != "0":
            s_logger.error("Selenese-runner-java terminated unexpectedly with code {}. Sending SIGTERM.".format(proc.poll()))
        else:
            s_logger.info("Selenese-runner-jar terminated with code {}. Sending SIGTERM.".format(proc.poll()))

        time.sleep(args_obj.wait*2)
        os.kill(os.getpid(), signal.SIGTERM)

    s_logger.info("Running selenese-runner.jar")
    global selrun_thr
    selrun_thr = threading.Thread(target=_run, name="Selenese Runner")
    selrun_thr.start()


def parse_args(args):
    parser = argparse.ArgumentParser(description='Main vilanoo2 proxy parameters')

    parser.add_argument("-v", "--verbose",
                        dest="verbose",
                        help="if set this flag enables debug output",
                        action="store_true")

    parser.add_argument("-b", "--bind",
                        dest="bind",
                        help="Vilanoo proxy binding IPv4 address. This address is also used for the proxy configuration of selenese-runner.",       
                        default='127.0.0.1',
                        metavar="IP",
                        type=str)
    
    parser.add_argument("-p", "--port",
                        dest="port",
                        help="TCP port for the Vilanoo proxy. This port is also used for the proxy configuration of selenese-runner.",  
                        default=8080,
                        metavar="PORT",
                        type=int)

    parser.add_argument("-M", "--mosgi-address",
                        dest="mosgi_addr",
                        help="MOSGI listening address.",
                        default='127.0.0.1',
                        metavar="IP",
                        type=str)

    parser.add_argument("-P", "--mosgi-port",
                        dest="mosgi_port",
                        help="MOSGI TCP port.",
                        default=8844,
                        metavar="PORT",
                        type=int)

    parser.add_argument("-s", "--sqlitedb",
                        dest="sqlitedb",
                        required=True,
                        help="SQLite3 DB file.",
                        metavar="PATH",
                        type=str)

    parser.add_argument("--no-mosgi",
                        dest="dismosgi",
                        help="By default, MOSGI is enabled. Use this option to disable MOSGI.",
                        action="store_false")

    parser.add_argument("-S", "--selenese",
                        dest="selenese",
                        help="Specify the selenese test case/suite to run. Vilanoo uses selenese-runner-java (modified to be interactive).",
                        metavar="PATH", type=str)

    parser.add_argument("-l", "--selenese-log",
                        dest="selenese_log",
                        help="the file which is the selense log for the current run",
                        metavar="PATH", type=str)

    parser.add_argument("--selenese-args",
                        dest="selenese_args",
                        help="Use this parameter to pass additional CLI arguments to selenese-runner-java",            
                        metavar="ARGS",
                        type=str)

    parser.add_argument("-w", "--wait",
                        dest="wait",
                        help="Waiting time in seconds before the next Selenese command is executed.",  
                        default='2',
                        metavar="SEC",
                        type=float)
    
    return parser.parse_args(args)


def main(args):
    global args_obj
    args_obj = parse_args(args)

    sqlitedb_init()

    if args_obj.dismosgi:
        connect_to_mosgi(args_obj.mosgi_addr, args_obj.mosgi_port)

    if args_obj.selenese:
        store_sel_commands(args_obj.selenese)
        start_selenese_runner(args_obj.selenese, args_obj.selenese_log)

    if args_obj.verbose:
        s_logger.setLevel(log.LEVELS[-1])
        m_logger.setLevel(log.LEVELS[-1])
        v_logger.setLevel(log.LEVELS[-1])

    start_proxy(args_obj.bind, args_obj.port)

    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
