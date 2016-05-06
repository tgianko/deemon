import clustering
import request


'''
This function first clusters the given requests
by similiar queries and then by similiar urls
'''
def perform_cluster_analysis(requests):
    request_cluster = Cluster(requests)
    return request_cluster.subcluster(request.request_queries_similar_p).subcluster(request.request_url_similar_p)
    
