import os
import sys
import sqlite3 as lite
import analysis_classes

state_change_keywords = ['DELETE','INSERT','DROP','SET','UPDATE']                            


def analyse_queries_of_requests(requests):
    queryhash_querystring_dict = {}
    queryhash_request_dict = {}
    queryhash_response_code_dict = {}
    for request in requests:
        for query in request.get_queries():
            query_hash = query.query_hash_normalized
            
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


def same_lists(elements,comp):
    sub_clusters = []
    sub_clusters.append( [ elements[0] ] )
    
    for element in elements[1:]:
        set_p = False
        for sub_cluster in sub_clusters:
            if comp(element,sub_cluster[0]):
                sub_cluster.append(element)
                set_p = True
                break
            
        if not set_p:
            sub_clusters.append( [ element ] )

    return sub_clusters
                
            
def main(argv=sys.argv):
    requests = analysis_classes.generate_request_objects(analysis_classes.get_relevant_request_ids(argv[1],
                                                                                                   state_change_keywords),
                                                         argv[1])
    '''
    for element in requests:
        print element
    queryhash_querystring_dict,queryhash_request_dict,queryhash_response_code_dict = analyse_queries_of_requests(requests)

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

    
    print ""
    print "---------- REQUEST CLASSES ---------"
    print ""
    cluster_similar = same_lists(requests,analysis_classes.requests_similar_p)
    for cluster in cluster_similar:
        for element in cluster:
            print "{}".format(element)
        cluster_equal = same_lists(cluster,analysis_classes.requests_equal_p)
        for clusterE in cluster_equal:
            for element in clusterE:
                print "--{}".format(element)
    '''
        
main()
