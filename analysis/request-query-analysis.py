import os
import sys
import sqlparse
import sqlite3 as lite

state_change_keywords = ['DELETE','INSERT','DROP','SET','UPDATE']

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


class Query:
    query_string=""
    query_type=""
    parse_tree=""

    def __init__(self,query_string,query_type):
        self.query_type = query_type
        self.query_string = query_string
        self.parse_tree = sqlparse.parse(query_string)


"""
NO WHITESPACES
"""
def semantically_equal_p(lhs,rhs):
    #generall idea -> kill all whitespaces,order identifierlists by alphabet, order comparison by alphabet
    #reconvert query to string - compare string
    raise NameException("NYI")
    
    
def queries_semantically_equal_p(lhs,rhs):
    raise NameException("NYI")


class Request:
    request_id = ""
    queries = []

    def __init__(self,request_id,queries):
        self.request_id = request_id
        self.queries = queries

    def __str__(self):
        counter_dic = {}
        for query in self.queries:
            if query.query_type in counter_dic:
                counter_dic[ query.query_type ] = counter_dic[ query.query_type ] + 1
            else:
                counter_dic[ query.query_type ] = 1
        return "[REQUEST ID:{0} QUERIES:{1}]" .format(self.request_id,counter_dic)


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
            requests.append( Request(key,queries) )

    return requests
                            

class Query_analysis_result:
    query = None
    request_ids = []

    def __init__(self,query,request_ids):
        self.query = query
        self.request_ids  = request_ids

    def __str__(self):
        return "[RESULT QUERY:{0} REQUEST_IDS:{1}]".format(self.query.query_string,self.request_ids)


def split_query_list(query_list,ref_query):
    neq = []
    eq = []
    for query in query_list:
        if (query.query_type == ref_query.query_type and
            queries_semantically_equal_p(query,ref_query)):
            eq.append(query)
        else:
            neq.append(query)
    return neq,eq


def update_query_analysis_result_m(result,request):
    neq_queries,eq_queries = split_query_list(request.queries,result.query)
        
    if len(neq_queries) == len(request.queries):
        return result
    else:
        request.queries = neq_queries
        result.request_ids.append(request.request_id)    
                 

def analyse_queries_of_requests_m(requests):
    results = []
    for idx,request in enumerate(requests):
        for query in request.queries:
            result = Query_analysis_result(query,[ request.request_id ])
            for rem_request in requests[idx:]:
                update_query_analysis_result_m(result,rem_request)
            results.append(result)

    return results


def main(argv=sys.argv):
    requests = generate_request_objects(get_relevant_request_ids(argv[1],state_change_keywords),
                                        argv[1])
    results = analyse_queries_of_requests_m(requests)
    for result in results:
        print result
    
            
        
main()
