import os
import sys
import sqlite3 as lite
import sql_query_normalization as normalizer

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

    def __init__(self,query_string,query_type):
        self.query_type = query_type
        self.query_string = query_string

    def __str__(self):
        return self.query_string

    
#def queries_equal_p(rhs,lhs):
    #print "comparing {0} with {1}".format(rhs,lhs)
#    return normalizer.normalized_query_hash(rhs) == normalizer.normalized_query_hash(lhs)


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

        annoying_buffer_string = "".join(["{0}\n".format(k) for k in self.queries])
        return "REQUEST ID:{0} \n QUERY_TYPES:{1} \n QUERIES:{2}".format(self.request_id,counter_dic,annoying_buffer_string)


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
                            

def analyse_queries_of_requests(requests):
    queryhash_querystring_dict = {}
    queryhash_request_dict = {}
    for request in requests:
        for query in request.queries:
            query_hash = normalizer.generate_normalized_query_hash(query.query_string)
            
            if not query_hash in queryhash_querystring_dict:
                queryhash_querystring_dict[query_hash] = query.query_string
                queryhash_request_dict[query_hash] = [ request.request_id ]
            else:
                queryhash_request_dict[query_hash].append(request.request_id)

    return queryhash_querystring_dict,queryhash_request_dict


def main(argv=sys.argv):
    requests = generate_request_objects(get_relevant_request_ids(argv[1],state_change_keywords),
                                        argv[1])
    queryhash_querystring_dict,queryhash_request_dict = analyse_queries_of_requests(requests)

    print ""
    print "------- REQUESTS ------"
    
    for request in requests:
        print request

    print ""
    print "------- QUERY-HASHS ------"

    
    for key,value in queryhash_querystring_dict.iteritems():
        print "HASH {0}\nQUERY {1}\n".format(key,value)


    print ""
    print "------- QUERY-HASHS TO REQUESTS------"

        
    for key,value in queryhash_request_dict.iteritems():
        print "HASH {0}\nREQUESTS {1}\n".format(key,value)

    
            
        
main()
