from py2neo import Relationship
from datamodel.core import *

def get_all_relationship_labels(graph, logger=None):
    return (map(lambda x: x[0],
                list(graph.run("MATCH ()-[r]-() RETURN distinct type(r)"))))


def get_all_relationship_labels_but(graph, but, logger=None):
    return (filter(lambda x: x not in but,
                   get_all_relationship_labels(graph, logger)))


def label_list_to_match_string(label_list):
    return "|".join(map(lambda x: ":{}".format(x), label_list))


def datapropagation_relation_count(graph):
    return list(graph.run("MATCH ()-[f:DATAPROPAGATION]->()\
 RETURN count(f)"))[0][0]
               

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
    old_count = datapropagation_relation_count(graph)

    for lhs, rhs in pairList:
        rel = Relationship(lhs, "DATAPROPAGATION", rhs)
        graph.create(rel)
    
    new_count = datapropagation_relation_count(graph)

    if logger is not None:
        logger.info("inserted {} dataprops".format(new_count - old_count))


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


def insert_data_flows(graph, projname, session, user, logger):
    insert_data_flow_selenese_HTTPRequest(graph, projname, session, user, logger)
    insert_data_flow_HTTPRequest_PHPSession(graph, projname, session, user, logger)
    insert_data_flow_HTTPRequest_SQLQuery(graph, projname, session, user, logger)


def insert_propagates_to(graph, rs, logger):

    rs = list(rs)
    i = 1
    for e in rs:
        if logger is not None:
            logger.info("Inserting relationship {}/{}".format(i, len(rs)))
        var1 =  Variable.select(graph).where(uuid=e[0]).first()
        var2 =  Variable.select(graph).where(uuid=e[1]).first()
        
        var1.PropagatesTo.add(var2)

        graph.push(var1)
        i+=1

def insert_backward_selenese_chains(graph, projname, session, user, logger):
    if logger is not None:
        logger.info("Adding Cross-Tier, Causal, Propagation Chains...")

    query = """MATCH (v1:Variable)-[:BELONGS_TO]->(e_sel1:Event {dm_type:"SeleneseCommand", projname: {projname}, session:{session}, user:{user}})-[:IS_FOLLOWED_BY*]->(e_sel2:Event {dm_type:"SeleneseCommand"})-[:CAUSED]->(e_http1:Event {dm_type:"HttpRequest"})<-[:BELONGS_TO]-(v2:Variable) 
                WITH v1, v2
               WHERE v1.value=v2.value
               RETURN v1.uuid,
                      v2.uuid"""

    rs = graph.run(query, projname=projname, session=session, user=user)
    
    insert_propagates_to(graph, rs, logger)



def insert_vertical_chains(graph, projname, session, user, logger):
    if logger is not None:
        logger.info("Adding Cross-Tier, Causal, Propagation Chains...")

    query = """MATCH (v_http:Variable)-[:BELONGS_TO]->(e_http:Event {projname: {projname}, session:{session}, user:{user}})-[:CAUSED*]->(e_final:Event)<-[:BELONGS_TO]-(v_final:Variable) 
                WITH v_http, v_final
               WHERE v_http.value=v_final.value 
              RETURN v_http.uuid, 
                     v_final.uuid"""
    
    rs = graph.run(query, projname=projname, session=session, user=user)
    
    insert_propagates_to(graph, rs, logger)

def insert_variables(graph, projname, session, user, logger):

    BLACKLIST = ["command-name", "command-value", 
                 "target-name", "target-value",
                 "value-name",
                 "HTTP-version",
                 "method",
                 "scheme",
                 "field-name",
                 "plaintext-body",
                 "Token.Operator.Comparison", "Token.Operator", 
                 "Token.Keyword", "Token.Keyword.DML",
                 "Token.Name.Builtin"]

    if logger is not None:
        logger.info("Deriving Variables from PTs...")
    
    query = """MATCH p1=(pt:ParseTree)-[:HAS_CHILD*]->(d:PTTerminalNode), 
                     p2=(pt)-[par:PARSES]->(e:Event) 
               WHERE size(d.symbol) > 0 
              UNWIND nodes(p1) AS t 
                WITH collect(t) AS sli, 
                     d, 
                     pt, 
                     e 
              RETURN e.dm_type AS dm_type,
                     e.seq AS seq,
                     reduce(s="(" + e.seq + ")" + e.dm_type, t IN sli | s + ">" + "(" + t.pos + ")" + coalesce(t.s_type, t.dm_type)) AS name, 
                     d.symbol AS value,
                     d.uuid AS has_value_uuid,
                     e.uuid AS belongs_to_uuid,
                     d.s_type as s_type"""
    rs = graph.run(query)
    rs = list(rs)
    
    def do_blacklist(e):
        return e["s_type"] not in BLACKLIST

    if logger is not None:
        logger.info("Applying blacklist of s_type")
    rs = filter(do_blacklist, rs)


    i = 1
    for e in rs:
        if e["s_type"] in BLACKLIST:
            if logger is not None:
                logger.info("Ignoring s_type {} {}/{}".format(e["s_type"], i, len(rs)))
            continue
        if logger is not None:
            logger.info("Inserting variable {}/{}".format(i, len(rs)))
        var =  Variable(projname, e["dm_type"], session, user, e["seq"], e["name"], e["value"])
        
        value = PTTerminalNode.select(graph).where(uuid=e["has_value_uuid"]).first()
        var.HasValue.add(value)

        event = Event.select(graph).where(uuid=e["belongs_to_uuid"]).first()
        var.BelongsTo.add(event)

        graph.push(var)
        i+=1
