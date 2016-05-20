import sys
import request
import clustering
import db_interface


state_change_keywords = ['DELETE','INSERT','DROP','SET','UPDATE']


def main(argv=sys.argv):
    requests = db_interface.get_relevant_requests(state_change_keywords,argv[1])
    print "This file describes the clustering of statechaning requests in sets as follows:"
    print "The first cluster describes set of all state changing requests"
    print "The first subsets describe a division by similar queries"
    print "(removed parameters,alphabetically ordered tree)"
    print "The second subsets describe a divison of the first subset by similar"
    print "urls (removed parameters,alphabetically ordered parameters)"

    for key,value in request.extract_query_dictionary(requests).items():
        print "{} : {}".format(key,value)

    for r in requests:
        print "Request id {} url {} code {} queries {}\n".format(r.request_id,r.url.geturl(),r.status_code,r.queries.keys())

    print ""

    print clustering.Cluster(requests).selfcluster(request.requests_queries_similar_p).selfcluster(request.requests_url_similar_p)


main()
