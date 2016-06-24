# -*- coding: utf-8 -*-
from proxy2 import *
import time

__DEBUG__     = True
__SIM_DELAY__ = True
__MOSGI__     = False

sqlitedb = os.path.expanduser("~") + "/.vilanoo/vilanoo.db"
sqlite_schema = "./proxyDbSchema.sql"
lock = threading.Lock()

"""mosgi_interface="127.0.0.1"
mosgi_port=9292
mosgi_start_command_byte=0
mosgi_finish_response_byte=2
mosgi_connection = socket.socket(socket.AF_INET,socket.SOCK_STREAM)
mosgi_connection.connect((mosgi_interface,mosgi_port))"""

def check_and_create():
    if os.path.exists(sqlitedb):
        pass
    else:
        con = lite.connect(sqlitedb)

        with con:            
            cur = con.cursor()
            f = open(sqlite_schema)
            with f:
                schema = f.read()
                cur.executescript(schema)


def insert_http_request(request,request_body):
    def parse_qsl(s):
        return '\n'.join("%-20s %s" % (k, v) for k, v in urlparse.parse_qsl(s, keep_blank_values=True))

    host = request.headers["host"][0]
    url = "{}{}".format(host, request.path)
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
        http_request_query_data = (datetime.datetime.now(),url,headers,body,method_type,cookies,"unknown")
        cur.execute("INSERT INTO http_requests (time,request_url,header,request_body,method_type,cookies,status_code) VALUES(?,?,?,?,?,?,?)",
                    http_request_query_data)
        request_id = cur.lastrowid
        global_last_request_id = request_id

    request.db_request_id = lambda: None
    setattr(request,'db_request_id',request_id)

    return request_id


def update_request_status(db_request_id,status_code):
    con = lite.connect(sqlitedb)
    
    with con:            
        cur = con.cursor()
        cur.execute("UPDATE http_requests SET status_code=? WHERE id=? ORDER BY id desc LIMIT 1",(status_code,db_request_id))

class VilanooProxyRequestHandler(ProxyRequestHandler):


    def do_GET(self):
        lock.acquire()
        ProxyRequestHandler.do_GET(self)

        lock.release()

    def request_handler(self, req, req_body):
        if __DEBUG__:        
            req_header_text = "%s %s %s\n%s" % (req.command, req.path, req.request_version, req.headers)
            print with_color(33, req_header_text)

        if __MOSGI__:
            print "===================start========================="
        
        return

    def response_handler(self, req, req_body, res, res_body):
        if __DEBUG__:
            res_header_text = "%s %d %s\n%s" % (res.response_version, res.status, res.reason, res.headers)       
            print with_color(32, res_header_text)
        
        if __MOSGI__ and self.request_relevant_p(req):
                print "mosgi..."
                db_id = insert_http_request(req,req_body)
                update_request_status(db_id,res.status)
                command = bytearray([mosgi_start_command_byte])
                mosgi_connection.send(command)
                #this should explode (booooom!) the int into 4 bytes and transmit them to mosgi
                request_id = bytearray( [ ((db_id>>24) & 0xff) ,
                                          ((db_id>>16) & 0xff), 
                                          ((db_id>>8) & 0xff), 
                                          (db_id & 0xff) ] )
                mosgi_connection.send(request_id)
                rcv = mosgi_connection.recv(1)
                res_header_text = "%s %d %s\n%s" % (res.response_version, res.status, res.reason, res.headers)       
                print with_color(32, res_header_text)
                print "===================finished======================"
            

        
        if __SIM_DELAY__:
            time.sleep(1)


        return

if __name__ == '__main__':

    test(HandlerClass=VilanooProxyRequestHandler)