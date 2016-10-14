from py2neo import Relationship


def get_all_relationship_labels(graph, logger=None):
    return (map(lambda x: x[0],
                list(graph.run("MATCH ()-[r]-() RETURN distinct type(r)"))))


def get_all_relationship_labels_but(graph, but, logger=None):
    return (filter(lambda x: x not in but,
                   get_all_relationship_labels(graph, logger)))


def label_list_to_match_string(label_list):
    return "|".join(map(lambda x: ":{}".format(x), label_list))
               

def get_data_flows(graph, rhs_data, rhs_start, lhs_start,
                   lhs_data, logger=None):
    if logger is not None:
        logger.info("selecting all {}->{} for {}->{}"
                    .format(lhs_data, rhs_data, lhs_start, rhs_start))
    
    allowed_con = label_list_to_match_string(
        get_all_relationship_labels_but(graph, ["CAUSED",
                                                "NEXT",
                                                "DATAPROPAGATION"]))

    query = "MATCH (rhs:{})<-[{}*]-(h:{})<-[c:CAUSED]-(s:{})-[{}*]\
->(lhs:{}) WHERE rhs.value = lhs.value return lhs,rhs".format(rhs_data,
                                                              allowed_con,
                                                              rhs_start,
                                                              lhs_start,
                                                              allowed_con,
                                                              lhs_data)

    return list(graph.run(query))
    # return []


def get_extended_data_flows_selenese_http(graph, rhs_data,
                                          lhs_data, logger=None):
    if logger is not None:
        logger.info("selecting all extended {}->{} for Selenese->HTTPRequest"
                    .format(lhs_data, rhs_data))

    allowed_con = label_list_to_match_string(
        get_all_relationship_labels_but(graph, ["CAUSED",
                                                "NEXT",
                                                "DATAPROPAGATION"]))

    query = "MATCH (rhs:{})<-[{}*]-(h:HTTPRequest)<-[c:CAUSED]-\
(:SeleneseCommand)<-[:NEXT*]-(s:SeleneseCommand)-[{}*]\
->(lhs:{}) WHERE rhs.value = lhs.value return lhs,rhs".format(rhs_data,
                                                              allowed_con,
                                                              allowed_con,
                                                              lhs_data)

    return list(graph.run(query))
    # return []


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


