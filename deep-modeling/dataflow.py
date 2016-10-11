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


def insert_data_flow_between_pairs(graph, pairList, logger=None):
    if logger is not None:
        logger.info("inserting {} data flow pairs".format(len(pairList)))

    for lhs, rhs in pairList:
        rel = Relationship(lhs, "DATAPROPAGATION", rhs)
        graph.create(rel)
    

def insert_data_flow_selenese_HTTPRequest(graph, logger):
    insert_data_flow_between_pairs(
        graph, get_data_flows(graph, "DataValue",
                              "HTTPRequest", "SeleneseCommand",
                              "DataValue", logger), logger)
    insert_data_flow_between_pairs(
        graph, get_data_flows(graph, "KeyValuePair",
                              "HTTPRequest", "SeleneseCommand",
                              "DataValue", logger), logger)
    insert_data_flow_between_pairs(
        graph, get_data_flows(graph, "DataValue",
                              "HTTPRequest", "SeleneseCommand",
                              "KeyValuePair", logger), logger)
    insert_data_flow_between_pairs(
        graph, get_data_flows(graph, "KeyValuePair",
                              "HTTPRequest", "SeleneseCommand",
                              "KeyValuePair", logger), logger)


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
