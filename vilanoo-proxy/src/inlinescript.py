'''
Created on Dec 1, 2015

@author: gianko #TODO email
@author:Simon Koch <s9sikoch@stud.uni-saarland.de>
'''
import urlparse
import sqlparse
import threading
import os
import socket
import datetime
import sqlite3 as lite

fmt = "{:>5}  {:>5}  {:<5} {:<30}"
sqlitedb = os.path.expanduser("~") + "/.vilanoo/vilanoo.db"
sqlite_schema = "./proxyDbSchema.sql"
mosgi_interface="127.0.0.1"
mosgi_port=9292
mosgi_start_command_byte=0
mosgi_finish_response_byte=2
mosgi_connection = socket.socket(socket.AF_INET,socket.SOCK_STREAM)
mosgi_connection.connect((mosgi_interface,mosgi_port))
mosgi_lock = threading.Lock()
global_last_request_id = -1


def serverconnect(context, server_conn):
    pass

def clientconnect(context, root_layer):
    pass


def mysqlproxy_is_running(context, proc):
    print fmt.format(*["SEL", "ST_CH", "METHD", "URL"])


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

        
def insert_http_request(stuff_we_really_do_not_care_about,request):
    host = request.headers["host"][0]
    url = "{}{}".format(host, request.path)
    headers = ""
    for key,value in request.headers.items():
        headers = headers + key + "=" + value + ";"
    body = "{0}".format(request.content)
    cookies =""
    for element in request.headers.lst:
        if element[0] == 'Cookie':
            cookies = cookies + element[1]
        
    cookies.replace(" ","")
            
    method_type = request.method
    check_and_create()    
    with sql_lock:        
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


"""
Handle input into the sqlite database
http_request is a string representing the http_request url
query_array is an array of tuples consisting of (query_type,query_string)
"""
sql_lock = threading.Lock()
def insert_http_query_data(query_array,request_id):
    print "insert query data"
    with sql_lock:        
        con = lite.connect(sqlitedb)        
        with con:            
            cur = con.cursor()            
            ##inserting the related sql_queries 
            sql_query_query_data = []
            counter = 0
            for query in query_array:
                insert_tuple = (request_id,counter,query[0],query[1])
                sql_query_query_data.append(insert_tuple)
                counter += 1
                
                cur.executemany("INSERT INTO sql_queries (http_request_id,query_counter,query_type,query_string) VALUES(?,?,?,?)",
                                sql_query_query_data)                


"""
This updates the status code to a given http_request
ATTENTION: This function makes the assumption that either no URL 
is entered twice or if so the request result is the same.
This is a horrible assumption! But it seems like a request in a browser might trigger
multiple requests and only a single response? WTF? Thus, to have a throughly consistent
database with response codes this is 
"""
def update_request_status(db_request_id,status_code):
    with sql_lock:        
        con = lite.connect(sqlitedb)
        
        with con:            
            cur = con.cursor()
            cur.execute("UPDATE http_requests SET status_code=? WHERE id=? ORDER BY id desc LIMIT 1",(status_code,db_request_id))


            
"""
Return true if the request can be async.
"""
def is_async_request(context, request):
    """
    host = request.headers["host"][0]
    
    # If request is for an external, let's send it async
    
    #if host not in ["192.168.56.4"]:
    #    return True
    
    url = "{}{}".format(host, request.path)
    urlp = urlparse.urlparse(url)
    ext  = urlp.path.split(".")[-1]
    if ext in ["jpg", "gif", "png", "ico",
               "css", "woff2",
               "js"]:
        return True
    """
    return False

"""
Do whatever you want with the queries, e.g., dynamic taint-analysis requests vs queries
"""
def process_queries(context, request, queries):
    sel   = 0
    st_ch = 0
    queries_array = []
    #print queries
    for q in queries:
        sql = sqlparse.parse(q)
        query_type = sql[0].tokens[0].value.upper()
        queries_array.append( (query_type,q) )
        if sql[0].tokens[0].value.upper() in ["SELECT"]:
            sel += 1
        if sql[0].tokens[0].value.upper() in ["UPDATE"]:
            st_ch += 1
        if sql[0].tokens[0].value.upper() in ["INSERT"]:
            st_ch += 1
        if sql[0].tokens[0].value.upper() in ["CREATE"]:
            st_ch += 1
        if sql[0].tokens[0].value.upper() in ["ALTER"]:
            st_ch += 1
        if sql[0].tokens[0].value.upper() in ["DROP"]:
            st_ch += 1
            
    insert_http_query_data(queries_array,request.db_request_id)
    host = request.headers["host"][0]
    url = "{}{}".format(host, request.path)
    print fmt.format(*[sel, st_ch, request.method, url])
    


#ACTHUNG -
#this function is called - tested sufficiently but threaded for no reason at all
#probably to mess with any logic using this function and expecting it not to
#threaded and completly out of sync with the rest of the stuff in this file - ACHTUNG!
def request(context, flow):
    pass

    
#ACHTUNG-
#THIS COULD BE ASYNC AS WELL - NO IDEA HOW TO HANDLE OR EVEN
#FIGURE THIS OUT o_O - ACHTUNG    
def response(context, flow):
    try:
        #well now that the queries are processed let call our lord and master mosgi
        command = bytearray([mosgi_start_command_byte])
        mosgi_connection.send(command)
        #this should explode the int into 4 bytes and transmit them to mosgi
        request_id = bytearray( [ ((flow.request.db_request_id>>24) & 0xff) ,
                             ((flow.request.db_request_id>>16) & 0xff), 
                             ((flow.request.db_request_id>>8) & 0xff), 
                             (flow.request.db_request_id & 0xff) ] )
        mosgi_connection.send(request_id)
        rcv = mosgi_connection.recv(1)
    except Exception as inst:
        print inst

    try:
        
        if "db_request_id" in flow.request.__dict__: #if no such id exists the response is of no interest for us
            request = flow.request
            db_request_id = flow.request.db_request_id
            host = request.headers["host"][0]
            url = "{}{}".format(host, request.path)
            http_response = flow.response
            update_request_status(db_request_id,http_response.code)
            
    except Exception as inst:
        print inst
        
    #update_request_status(url,http_response.status_code)

