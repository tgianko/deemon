#! /usr/bin/python

"""
This code is derived from mitmproxy 0.10.1. 

Synchronous HTTP requests are implemented by hooking new functions into mitmproxy.

"""


import sys, signal
from libmproxy import proxy, dump, cmdline 
import libmproxy.version, netlib.version
import argparse
import subprocess
import threading
import time
from utils import _host_in_scope, _uninteresting_URL
import os, os.path
import config

class MySQLProxyError(Exception):
    pass

class MySQLProxyReader():
        
    def __init__(self, options):
        self.bin  = options.mysql_proxy_bin
        self.lhost = options.addr # standard mitmproxy CLI param
        self.lport = options.mysql_proxy_port
        self.rhost = options.mysql_server_host
        self.rport = options.mysql_server_port
        self.verbose = options.verbose
        self.script = None

        if options.state_changing_reqs:
            self.script = os.path.join(config.mysql_proxy_scripts, "logquery_stdout.lua")
        
        self._query = []
        
        def proxy():               
            command = "{bin} -P {lhost}:{lport} -b {rhost}:{rport}".format(bin=self.bin, rhost=self.rhost, rport=self.rport, lhost=self.lhost, lport=self.lport)
            if self.script is not None:
                command += " -s {script}".format(script=self.script)
            
            if self.verbose > 0:
                print "mysql-proxy command:", command

            self._proc = subprocess.Popen(command.split(" "), bufsize=0, stdout=subprocess.PIPE)
            for line in iter( self._proc.stdout.readline, b""):
                if self._proc.poll() is not None:
                    break
                
                self._query.append(line)
            
            if self._proc.poll() is not None:
                #raise MySQLProxyError("mysql-proxy ended unexpectedly with code {}".format(self._proc.poll()))
                print >> sys.stderr, "mysql-proxy ended unexpectedly with code {}. Sending SIGTERM.".format(self._proc.poll())
                os.kill(os.getpid(), signal.SIGTERM)
         
        
        
        self._proc = threading.Thread(target=proxy, name="MYSQL Proxy process")
        self._proc.start()
        
    
    def get_queries(self):
        time.sleep(0.5)
        out = self._query[0:]
        self._query = self._query[len(out):]
        return out




# ------------------------ HACK VIA HOOK --------------------------
# storing original send to _send
proxy.ServerConnection._send = proxy.ServerConnection.send

# this is our lock which will do the magic
lock = threading.Lock()
mysqlproxy = None
options = None # this will make option global.

def _is_select_query(query):
    return "SELECT" in query

def _is_state_changing_query(query):
    # Very unsophisticated. We could parse SQL and do more here.
    return "UPDATE" in query or "INSERT" in query or "CREATE" in query or "ALTER" in query or "DROP" in query

def _send_decorator(self, request):
    # if mysql proxy is not running, then  
    if mysqlproxy is None:
        proxy.ServerConnection._send(self, request)
        return
    
    # OPTIMIZATION: 
    # We do not want to linearize all requests (e.g., images). 
    # We apply two filters: 
    # 1) when requests are for a third-party server, we do not synchronize
    # 2) when requests are for uninteresting resources, idem
    #
    # TODO: make filters (e.g., host_in_scope and URL) user customizable
    #
    
    host = request.headers["host"][0]

    url = "{}{}".format(host, request.path)
    if not _host_in_scope(host) or _uninteresting_URL(url):
        proxy.ServerConnection._send(self, request)
        return

    lock.acquire()
    proxy.ServerConnection._send(self, request)
    buff = mysqlproxy.get_queries()

    """
    TODO: extract SQL processing to an external function and use a SQL parser,
    e.g., do_process(request, buff) where do_process is external
    
    --FROM HERE--
    """  
    fmt = ""
    vals = []
    if options.state_changing_reqs: 
        fmt += "{:>5}"
        state_changing = len(filter(_is_state_changing_query, buff)) 
        vals.append(str(state_changing))
    if options.all_reqs:
        fmt += " {:>5}"
        select = len([l for l in buff if _is_select_query(l)])
        vals.append(str(select))
    
    fmt += " {:<5} {:<30}"
    vals.append(request.method)
    vals.append(url)
    print fmt.format(*vals)
    """
    --TO HERE--
    """
    
    lock.release()

# install our send function
proxy.ServerConnection.send = _send_decorator
# ------------------------ END OF HACK --------------------------

if __name__ == '__main__':
    # We don't introduce backward-incompatible changes in patch versions. Only consider major and minor version.
    if netlib.version.IVERSION[:2] != libmproxy.version.IVERSION[:2]:
        print >> sys.stderr, ("warning: You are using mitmdump %s with netlib %s. "
                              "Most likely, that doesn't work - please upgrade!") % (libmproxy.version.VERSION,
                                                                                  netlib.version.VERSION)
    parser = argparse.ArgumentParser(usage = "%(prog)s [options] [filter]")
    parser.add_argument('--version', action='version', version="mitmdump" + " " + libmproxy.version.VERSION)
    cmdline.common_options(parser)
    parser.add_argument(
        "--keepserving",
        action="store_true", dest="keepserving", default=False,
        help="Continue serving after client playback or file read. We exit by default."
    )
    parser.add_argument('args', nargs=argparse.REMAINDER)
    
    # mysql-proxy options
    group = parser.add_argument_group("MySQL Proxy options")
    
    group.add_argument(
        "--mysql-proxy-bin",
        action="store", type = str, dest="mysql_proxy_bin", default="mysql-proxy",
        help = "mysql-proxy binary location"
    )
    group.add_argument(
        "--mysql-proxy-port",
        action="store", type = int, dest="mysql_proxy_port", default=4040,
        help = "mysql-proxy TCP port  (default 4040)"
    )
    group.add_argument(
        "--mysql-server-host",
        action="store", type = str, dest="mysql_server_host", default='localhost',
        help = "MySQL server address (default localhost)"
    )
    group.add_argument(
        "--mysql-server-port",
        action="store", type = int, dest="mysql_server_port", default=3306,
        help = "MySQL server TCP port (default 3306)"
    )

    group = parser.add_argument_group("Web Application Analyses Options")
    group.add_argument(
        "--state-changing-requests",
        action="store_true", dest="state_changing_reqs", default=False,
        help = "Show only state-changing HTTP requests"
    )
    group.add_argument(
        "--all-requests",
        action="store_true", dest="all_reqs", default=False,
        help = "Show all requests including state-changing ones"
    )


    options = parser.parse_args()

    if options.quiet:
        options.verbose = 0

    proxyconfig = proxy.process_proxy_options(parser, options)
    if options.no_server:
        server = proxy.DummyServer(proxyconfig)
    else:
        try:
            server = proxy.ProxyServer(proxyconfig, options.port, options.addr)
        except proxy.ProxyServerError, v:
            print >> sys.stderr, "mitmdump:", v.args[0]
            sys.exit(1)
    try:
        dumpopts = dump.Options(**cmdline.get_common_options(options))
    except cmdline.OptionException, v:
        parser.error(v.message)
    dumpopts.keepserving = options.keepserving

    if options.args:
        filt = " ".join(options.args)
    else:
        filt = None

    try:
        def cleankill(*args, **kwargs):
            print >> sys.stderr, "SIGTERM. Terminating."
            m.shutdown()
        signal.signal(signal.SIGTERM, cleankill)
        mysqlproxy = MySQLProxyReader(options) 
        
        m = dump.DumpMaster(server, dumpopts, filt) #, sqlproxy=MySQLProxyReader())
        m.run()
    except dump.DumpError, e:
        print >> sys.stderr, "mitmdump:", e
        sys.exit(1)
    except KeyboardInterrupt, e:
        print >> sys.stderr, "keyboard interrupt. Terminating.", e

