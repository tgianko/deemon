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


class Request:
    request_id = ""
    queries = []
    status_code = ""

    def __init__(self,request_id,status_code,queries):
        self.request_id = request_id
        self.queries = queries
        self.status_code = status_code

    def __str__(self):
        counter_dic = {}
        for query in self.queries:
            if query.query_type in counter_dic:
                counter_dic[ query.query_type ] = counter_dic[ query.query_type ] + 1
            else:
                counter_dic[ query.query_type ] = 1

        annoying_buffer_string = "".join(["{0}\n".format(k) for k in self.queries])
        return "REQUEST ID:{0} STATUS_CODE:{1} \n QUERY_TYPES:{2} \n QUERIES:{3}".format(self.request_id,self.status_code,counter_dic,annoying_buffer_string)


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
            cur.execute("SELECT status_code FROM http_requests WHERE id = ?",(key,))
            result = cur.fetchone()
            requests.append( Request(key,result[0],queries) )

    return requests
                            

def analyse_queries_of_requests(requests):
    queryhash_querystring_dict = {}
    queryhash_request_dict = {}
    queryhash_response_code_dict = {}
    for request in requests:
        for query in request.queries:
            query_hash = normalizer.generate_normalized_query_hash(query.query_string)
            
            if not query_hash in queryhash_querystring_dict:
                queryhash_querystring_dict[query_hash] = query.query_string
                queryhash_request_dict[query_hash] = [ request.request_id ]
                queryhash_response_code_dict[query_hash] = [ request.status_code ]
            else:
                queryhash_request_dict[query_hash].append(request.request_id)
                queryhash_response_code_dict[query_hash].append(request.status_code)

    return queryhash_querystring_dict,queryhash_request_dict,queryhash_response_code_dict


def print_status_code_distr(queryhash,queryhash_response_code_dict):
    response_codes = sorted(queryhash_response_code_dict[queryhash])
    response_codes_length = len(response_codes)
    dictionary = {}
    for element in response_codes:
        if element in dictionary:
            dictionary[element] = dictionary[element] + 1
        else:
            dictionary[element] = 1

            
    for key,value in dictionary.items():
        print "{} : {:5.2%} [{}]".format(key,((1.0 * value)/response_codes_length),value)
            
    

def main(argv=sys.argv):
    requests = generate_request_objects(get_relevant_request_ids(argv[1],state_change_keywords),
                                        argv[1])
    queryhash_querystring_dict,queryhash_request_dict,queryhash_response_code_dict = analyse_queries_of_requests(requests)

    print ""
    print "------- REQUESTS ------"
    
    for request in requests:
        print request

    print ""
    print "------- QUERY-HASHS ------"

    
    for key,value in queryhash_querystring_dict.iteritems():
        print "HASH {0}\nQUERY {1}".format(key,value)
        print_status_code_distr(key,queryhash_response_code_dict)
        print ""


    print ""
    print "------- QUERY-HASHS TO REQUESTS------"

        
    for key,value in queryhash_request_dict.iteritems():
        print "HASH {0}\nREQUESTS {1}\n".format(key,value)

    
            
        
main()
