import os
import sqlite3 as lite
import request as package_request


def get_relevant_request_ids(sqlitedb_path,keywords):
    if not os.path.exists(sqlitedb_path):
        raise NameError("'" + sqlitedb_path + "' no such database exists!")
    
    request_ids = {}

    con = lite.connect(sqlitedb_path)

    with con:
        cur = con.cursor()
        
        for word in keywords:
            cur.execute('SELECT id FROM http_requests WHERE id IN (SELECT http_request_id FROM sql_queries WHERE query_type = ?)',(word,))
            for result in cur.fetchall():
                if result[0] in request_ids:
                    request_ids[ result[0] ] = request_ids[ result[0] ] + ";" + word
                else:
                    request_ids[ result[0] ] = word

    return request_ids


def get_relevant_requests(keywords,sqlitedb_path) :
    request_id_dictionary = get_relevant_request_ids(sqlitedb_path,keywords)
    if not os.path.exists(sqlitedb_path):
        raise NameError("'" + sqlitedb_path + "' no such database exists!")
    
    con = lite.connect(sqlitedb_path)
    requests = []

    with con:
        cur = con.cursor()

        for key,value in request_id_dictionary.items():
            queries = []
            for keyword in value.split(";"):
                cur.execute("SELECT query_string FROM sql_queries WHERE http_request_id=? AND query_type=?",
                            (key,keyword))
                for result in cur.fetchall():
                    queries.append( package_request.Query(result[0],keyword) )
            cur.execute("SELECT status_code,request_url FROM http_requests WHERE id = ?",(key,))
            result = cur.fetchone()
            request = None
            request = package_request.Request(key,result[0],result[1],queries)
            #print request.queries.keys()
            requests.append( request )

    return requests
