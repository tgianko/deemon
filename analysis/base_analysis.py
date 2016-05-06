import request


class QueryBaseAnalysis(object):
    
    queryhash_qerystring_dict    = None
    queryhash_request_dict       = None
    queryhash_response_code_dict = None

    def __init__(self,requests):
        self.queryhash_querystring_dict = {}
        self.queryhash_request_dict = {}
        self.queryhash_response_code_dict = {}
        for request in requests:
            for query in request.get_queries():
                query_hash = query.query_hash_normalized
                
                if not query_hash in queryhash_querystring_dict:
                    self.queryhash_querystring_dict[query_hash] = query.query_string
                    self.queryhash_request_dict[query_hash] = [ request.request_id ]
                    self.queryhash_response_code_dict[query_hash] = [ request.status_code ]
                else:
                    self.queryhash_request_dict[query_hash].append(request.request_id)
                    self.queryhash_response_code_dict[query_hash].append(request.status_code)



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
