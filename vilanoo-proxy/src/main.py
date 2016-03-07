#! /usr/bin/python

"""
This code is derived from mitmdump 0.10.1. 

Synchronous HTTP requests are implemented by hooking new functions into mitmproxy.

"""



import sys, signal

print sys.path
#import mitmproxy
#__package__ = "mitmproxy"

from libmproxy import proxy, dump, cmdline 
import libmproxy.version, netlib.version
import argparse
import subprocess
import threading
import time
import os.path
import config

# ------------------------ HACK VIA HOOK --------------------------


"""
MySQL*: classes to handler external tool.

The global var mysqlproxy will keep an instance of the MySQLProxyReader whereas
the master global var will keep a pointer to the DumpMaster instance (See mitmproxy
doc/src). We need an instance of the DumpMaster to access to the Script interface
and run our functions to handle inline SQL events. Ah.. and we need also to access
to the CLI options. 
"""

# global stuff
mysqlproxy = None
master = None
options = None   

class MySQLProxyError(Exception):
    pass

class MySQLProxyReader():
        
    def __init__(self, options):
        self.bin = options.mysql_proxy_bin
        self.lhost = options.addr  # standard mitmproxy CLI param
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
            time.sleep(0.5)
            """
            Let's notify user inlinescript that the proxy is up and running.
            """
            for s in master.scripts:
                suc, ret = s.run('mysqlproxy_is_running', self._proc)
                if suc is False:
                    raise ret
                    continue

            """
            Now we read the stream of SQL queries
            """            
            for line in iter(self._proc.stdout.readline, b""):
                if self._proc.poll() is not None:
                    break
                
                self._query.append(line)
            
            if self._proc.poll() is not None:
                # raise MySQLProxyError("mysql-proxy ended unexpectedly with code {}".format(self._proc.poll()))
                print >> sys.stderr, "mysql-proxy ended (unexpectedly?!) with code {}. Sending SIGTERM.".format(self._proc.poll())
                os.kill(os.getpid(), signal.SIGTERM)
         
        
        
        self._proc = threading.Thread(target=proxy, name="MYSQL Proxy process")
        self._proc.start()
        
    def get_queries(self):
        time.sleep(0.5)
        out = self._query[0:]
        self._query = self._query[len(out):]
        return out

"""
Hacks to hook functions directly into mitmdump/proxy. Our trick here 
is to hook our own send function in proxy.ServerConnection.send. We
use the function decorator pattern. 

NOTE: the order of these operations is fundamental.
"""

proxy.ServerConnection._send = proxy.ServerConnection.send

# this is our lock which will do the magic
lock = threading.Lock()

def _send_decorator(self, request):
    # if mysql proxy is not running, then  
    if mysqlproxy is None:
        proxy.ServerConnection._send(self, request)
        return

    """
    Run user provided function to decide whether to
    keep the operation async or not. Note that at
    this point we are not yet inside the lock.
    """
    for s in master.scripts:
        suc, ret = s.run('is_async_request', request)
        if suc is False:
            raise ret
            continue
        
        if ret is True:
            proxy.ServerConnection._send(self, request)
            return

    """
    Process the incoming SQL queries.
    """
    lock.acquire()
    proxy.ServerConnection._send(self, request)
    buff = mysqlproxy.get_queries()
    
    for s in master.scripts:
        queries = []
        sql = ""
        for line in buff:
            if ";QUERY;" in line:
                if len(sql) > 0:
                    queries.append(sql)
                sql = line.split(";QUERY;")[1]
            else:
                sql += line
                
        suc, ret = s.run('process_queries', request, queries)
        
        if suc is False:
            raise ret
    
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
    parser = argparse.ArgumentParser(usage="%(prog)s [options] [filter]")
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
        action="store", type=str, dest="mysql_proxy_bin", default="mysql-proxy",
        help="mysql-proxy binary location"
    )
    group.add_argument(
        "--mysql-proxy-port",
        action="store", type=int, dest="mysql_proxy_port", default=4040,
        help="mysql-proxy TCP port  (default 4040)"
    )
    group.add_argument(
        "--mysql-server-host",
        action="store", type=str, dest="mysql_server_host", default='localhost',
        help="MySQL server address (default localhost)"
    )
    group.add_argument(
        "--mysql-server-port",
        action="store", type=int, dest="mysql_server_port", default=3306,
        help="MySQL server TCP port (default 3306)"
    )

    group = parser.add_argument_group("Web Application Analyses Options")
    group.add_argument(
        "--state-changing-requests",
        action="store_true", dest="state_changing_reqs", default=False,
        help="Show only state-changing HTTP requests"
    )
    group.add_argument(
        "--all-requests",
        action="store_true", dest="all_reqs", default=False,
        help="Show all requests including state-changing ones"
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
        
        m = dump.DumpMaster(server, dumpopts, filt)  # , sqlproxy=MySQLProxyReader())
        master = m  # make it globally visible
        m.run()
    except dump.DumpError, e:
        print >> sys.stderr, "mitmdump:", e
        sys.exit(1)
    except KeyboardInterrupt, e:
        print >> sys.stderr, "keyboard interrupt. Terminating.", e

