import os
import sys
import hashlib
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

    def __str__(self):
        return self.query_string


def remove_whitespaces(tree):
    if tree.is_group():
        tree.tokens = [remove_whitespaces(element) for element in tree.tokens if not element.is_whitespace()]

    return tree


def remove_rhs_values_sub(element):
    if type(element) is sqlparse.sql.Comparison:
        element.tokens.remove(element.right) #delete any value as this might be different to eq queries in diff context
        return element

    if type(element) is sqlparse.sql.Assignment:
        raise NameError('Unexpected assignment operator SQL')

    return element
    

def remove_rhs_values(tree):

    if tree.is_group():
        #print tree.tokens
        tree.tokens = [remove_rhs_values(element) for element in [remove_rhs_values_sub(element) for element in tree.tokens]]
        return tree #this return just looks pretty

    return tree

        
def order_alphabetically(tree):
    if tree.is_group():
        tree.tokens = sorted([order_alphabetically(element) for element in tree.tokens], key=lambda el: el.value)

    return tree


def normalize_query_syntax_tree(tree):
    return order_alphabetically(remove_rhs_values(remove_whitespaces(tree)))
    #return remove_rhs_values(remove_whitespaces(tree))
    #return remove_rhs_values(tree)

def normalized_query_hash(query_string):
    hashlib.md5(normalize_query_syntax_tree(sqlparse.parse(query_string)[0]).__str__()).hexdigest()

    
def queries_equal_p(rhs,lhs):
    #print "comparing {0} with {1}".format(rhs,lhs)
    return normalized_query_hash(rhs) == normalized_query_hash(lhs)


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
                            

class Query_analysis_result:
    query = None
    request_ids = []

    def __init__(self,query,request_ids):
        self.query = query
        self.request_ids  = request_ids

    def __str__(self):
        return "[RESULT QUERY:{0} REQUEST_IDS:{1}]".format(self.query.query_string,self.request_ids)


#this stuff can (now) be done more efficient by just using a hashtable for the query categorization
#but I am lazy!
def split_query_list(query_list,ref_query):
    neq = []
    eq = []
    for query in query_list:
        if (query.query_type == ref_query.query_type and
            queries_equal_p(query.query_string,ref_query.query_string)):
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

    print ""
    print "------- REQUESTS ------"
    
    for request in requests:
        print request

    print ""
    print "------- RESULTS ------"
    
    results = analyse_queries_of_requests_m(requests)
    for result in results:
        print result

    
            
        
main()
