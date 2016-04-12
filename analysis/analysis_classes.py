import os
import sys
import sqlite3 as lite
import sql_query_normalization as normalizer


class Query(object):
    query_string=""
    query_type=""
    query_hash=""
    query_hash_normalized=""

    def __init__(self,query_string,query_type):
        self.query_type   = query_type
        self.query_string = query_string
        self.query_hash   = normalizer.generate_query_hash(query_string)
        self.query_hash_normalized = normalizer.generate_normalized_query_hash(query_string) #order is IMPORTANT

    def __str__(self):
        return self.query_string


class Request(object):
    request_id = ""
    queries = None
    status_code = ""
    request_url = ""

    def __init__(self,request_id,status_code,request_url,queries):
        self.queries = {}
        self.request_id = request_id
        for query in queries:
            self.queries[ query.query_hash_normalized ] = query
        self.status_code = status_code
        self.request_url = request_url


    def __str__(self):
        return "REQUEST ID:{0} STATUS_CODE:{1} URL:{2} Q:{3}".format(self.request_id,self.status_code,self.request_url,self.queries.keys())

    def get_queries(self):
        ret = []
        for key,value in self.queries.items():
            ret.append(value)
        return ret

    def query_types(self):
        counter_dic = {}
        for query in self.queries:
            if query.query_type in counter_dic:
                counter_dic[ query.query_type ] = counter_dic[ query.query_type ] + 1
            else:
                counter_dic[ query.query_type ] = 1
        return counter_dic


def requests_equal_p(request_lhs,request_rhs):
    query_hashes_lhs = [ query.query_hash for query in request_lhs.get_queries() ]
    query_hashes_rhs = [ query.query_hash for query in request_rhs.get_queries() ]
    return set(query_hashes_lhs) == set(query_hashes_rhs)


def requests_similar_p(request_lhs,request_rhs):
    normalized_query_hashes_lhs = [ query.query_hash_normalized for query in request_lhs.get_queries() ]
    normalized_query_hashes_rhs = [ query.query_hash_normalized for query in request_rhs.get_queries() ]
    return set(normalized_query_hashes_lhs) == set(normalized_query_hashes_rhs)


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


def generate_request_objects(request_id_dictionary,sqlitedb_path) :
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
                    queries.append( Query(result[0],keyword) )
            cur.execute("SELECT status_code,request_url FROM http_requests WHERE id = ?",(key,))
            result = cur.fetchone()
            request = None
            request = Request(key,result[0],result[1],queries)
            #print request.queries.keys()
            requests.append( request )

    return requests
