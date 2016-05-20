#import os
#import sys
import urlparse
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
    url = None
    post_parameters = None #NYI

    def __init__(self,request_id,status_code,request_url,queries):
        self.queries = {}
        self.request_id = request_id
        for query in queries:
            self.queries[ query.query_hash_normalized ] = query
        self.status_code = status_code
        self.url = urlparse.urlparse(request_url)


    def __str__(self):
        return '{}'.format(self.request_id)
        #return "REQUEST ID:{0} STATUS_CODE:{1} URL:{2} Q:{3}".format(self.request_id,self.status_code,self.url.geturl(),self.queries.keys())

    def __repr__(self):
        return self.__str__()


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


    def get_url_no_param_values(self):
        if self.url.params != '' or self.url.fragment != '':
            raise self.url

        url_buffer = "{}://{}{}?".format(self.url.scheme,self.url.netloc,self.url.path)
        for element in sorted([ elem.split("=")[0] for elem in self.url.query.split("&") ]):
            url_buffer = url_buffer + "{}=&".format(element)

        return url_buffer
        


def requests_queries_equal_p(request_lhs,request_rhs):
    query_hashes_lhs = [ query.query_hash for query in request_lhs.get_queries() ]
    query_hashes_rhs = [ query.query_hash for query in request_rhs.get_queries() ]
    return set(query_hashes_lhs) == set(query_hashes_rhs)


def requests_queries_similar_p(request_lhs,request_rhs):
    normalized_query_hashes_lhs = [ query.query_hash_normalized for query in request_lhs.get_queries() ]
    normalized_query_hashes_rhs = [ query.query_hash_normalized for query in request_rhs.get_queries() ]
    return set(normalized_query_hashes_lhs) == set(normalized_query_hashes_rhs)


def requests_url_equal_p(request_lhs,request_rhs):
    return request_lhs.url.geturl() == request_rhs.url.geturl()


def requests_url_similar_p(request_lhs,request_rhs):
    return request_lhs.get_url_no_param_values() ==  request_rhs.get_url_no_param_values()


def extract_query_dictionary(requests):
    complete_dict = {}
    for request in requests:
        for key,value in request.queries.items():
            if not key in complete_dict:
                complete_dict[key] = value.query_string

    return complete_dict
