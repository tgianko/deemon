from py2neo import Relationship


def get_data_flows(graph, rhs_data, rhs_start, lhs_start,
                   lhs_data, logger=None):
    if logger is not None:
        logger.info("selecting all {}->{} for {}->{}"
                    .format(lhs_data, rhs_data, lhs_start, rhs_start))

    query = "MATCH (rhs:{})<-[rr*1..12]\
-(h:{})<-[c:CAUSED]-(s:{})-[rl*1..12]\
->(lhs:{}) WHERE NOT (s)-[:NEXT|CAUSED]->()-[*]->(lhs)\
 AND NOT (h)-[:NEXT|CAUSED]->()-[*]->(rhs) AND\
 rhs.value = lhs.value return lhs,rhs".format(rhs_data, rhs_start,
                                              lhs_start, lhs_data)

    return list(graph.run(query))


def get_extended_data_flows_selenese_http(graph, rhs_data,
                                          lhs_data, logger=None):
    if logger is not None:
        logger.info("selecting all extended {}->{} for Selenese->HTTPRequest"
                    .format(lhs_data, rhs_data))

    query = "MATCH (rhs:{})<-[rr*1..12]\
-(h:HTTPRequest)<-[c:CAUSED]-(:SeleneseCommand)<-[:NEXT*0..100]-(s:SeleneseCommand)-[rl*1..12]\
->(lhs:{}) WHERE NOT (s)-[:NEXT|CAUSED]->()-[*]->(lhs)\
 AND NOT (h)-[:NEXT|CAUSED]->()-[*]->(rhs) AND\
 rhs.value = lhs.value return lhs,rhs".format(rhs_data, lhs_data)

    return list(graph.run(query))


def insert_data_flow_between_pairs(graph, pairList, logger=None):
    if logger is not None:
        logger.info("inserting {} data flow pairs".format(len(pairList)))

    for lhs, rhs in pairList:
        rel = Relationship(lhs, "DATAPROPAGATION", rhs)
        graph.create(rel)
    

def insert_data_flow_selenese_HTTPRequest(graph, logger):
    insert_data_flow_between_pairs(
        graph, get_extended_data_flows_selenese_http(
            graph, "DataValue", "DataValue", logger), logger)
    insert_data_flow_between_pairs(
        graph, get_extended_data_flows_selenese_http(
            graph, "DataValue", "KeyValuePair", logger), logger)
    insert_data_flow_between_pairs(
        graph, get_extended_data_flows_selenese_http(
            graph, "KeyValuePair", "DataValue", logger), logger)
    insert_data_flow_between_pairs(
        graph, get_extended_data_flows_selenese_http(
            graph, "KeyValuePair", "KeyValuePair", logger), logger)


def insert_data_flow_HTTPRequest_SQLQuery(graph, logger):
    insert_data_flow_between_pairs(
        graph, get_data_flows(graph, "SQLToken",
                              "SQLQuery", "HTTPRequest",
                              "DataValue", logger), logger)
    insert_data_flow_between_pairs(
        graph, get_data_flows(graph, "SQLToken",
                              "SQLQuery", "HTTPRequest",
                              "KeyValuePair", logger), logger)


def insert_data_flow_HTTPRequest_PHPSession(graph, logger):
    insert_data_flow_between_pairs(
        graph, get_data_flows(graph, "DataValue",
                              "PHPSession", "HTTPRequest",
                              "DataValue", logger), logger)
    insert_data_flow_between_pairs(
        graph, get_data_flows(graph, "DataValue",
                              "PHPSession", "HTTPRequest",
                              "KeyValuePair", logger), logger)


def insert_data_flows(graph, logger):
    insert_data_flow_selenese_HTTPRequest(graph, logger)
    insert_data_flow_HTTPRequest_PHPSession(graph, logger)
    insert_data_flow_HTTPRequest_SQLQuery(graph, logger)
