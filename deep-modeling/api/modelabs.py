from datamodel.core import Event, ParseTree, DFAState, DFAStateTransition, AbstractParseTree
from dm_types import *
import sqlnorm
import hashlib


def get_all_sql_queries_of(xdebug_event, graph, logger=None, projname=None):
    query = """MATCH (q:ParseTree {dm_type: {sqltype}})-[:PARSES]->(e:Event {dm_type: {xdebug}, uuid:{uuid}})
               RETURN DISTINCT q.uuid AS uuid"""

    data = {
        "sqltype": SQL,
        "xdebug": XDEBUG,
        "uuid": xdebug_event.uuid
    }

    rs = graph.run(query, data)

    rs = list(rs)

    final_list = list()
    for q in rs:
        query = ParseTree.select(graph).where(uuid=q["uuid"]).first()
        final_list.append(query)
    return final_list


# TODO: If this function is still in use we should rename it
def get_all_sql_queries_of_old(xdebug_event, graph, logger=None):
    sqlQueries = ParseTree.select(graph).where("_.dm_type='{}'".format(ABSQUERY))
    final_list = list()
    for query in list(sqlQueries):
        if list(query.Parses)[0].uuid == xdebug_event.uuid:
            final_list.append(query)

    return final_list


# TODO: This function is based on the model without AbstractSQLParseTrees, changing
#  this would conform more closely to our chosen model. But for now 'Never touch
#  a running system'
"""
def get_summary_sql_queries_abstraction_hash(httpRequestEvent, graph, logger=None):
    if list(httpRequestEvent.Caused) == []:
        if logger is not None:
            logger.info("{} has no xdebug".format(httpRequestEvent))
        return hashlib.md5("").hexdigest()
    else:
        xdebug = list(httpRequestEvent.Caused)[0]
        query_hashes = list()
        for queryParseTree in get_all_sql_queries_of(xdebug, graph, logger):
            query_hashes.append(sqlnorm.generate_normalized_query_hash(queryParseTree.message))

            query_hashes.sort()
        return hashlib.md5("".join(query_hashes)).hexdigest()
"""


def get_summary_sql_queries_abstraction_hash(httpRequestEvent, graph, logger=None):
    # TODO: check this query I expect AbstrQ -> Q -> Xdebug -> Request
    query = """MATCH (a:AbstractParseTree {dm_type:'SqlQuery'})\
-[]->()-[]->()-[]->(:Event {uuid:{uuid}}) RETURN a"""
    rs = graph.run(query, uuid=httpRequestEvent.uuid)
    query_hash_array = list()
    for abstract_query in list(rs):
        query_hash_array.append(abstract_query['a']['message'])
    return hashlib.md5("".join(query_hash_array)).hexdigest()


# TODO: check if url normalization in abstractEvent is proper
def get_http_request_method_url_abstraction_hash(httpRequestEvent, graph, logger=None):
    query = """MATCH (a:AbstractEvent)-[:ABSTRACTS]->(:Event {uuid:{uuid}}) RETURN a"""
    rs = graph.run(query, uuid=httpRequestEvent.uuid)
    rs = list(rs)
    assert(len(rs) == 1)
    return hashlib.md5(rs[0]['a']['message']).hexdigest()


def get_http_abstraction_hash(httpRequestEvent, graph, logger=None):
    query_abstraction_hash = get_summary_sql_queries_abstraction_hash(httpRequestEvent,
                                                                      graph,
                                                                      logger)
    method_url_abstraction_hash = get_http_request_method_url_abstraction_hash(httpRequestEvent,
                                                                               graph,
                                                                               logger)
    hash_collection = [query_abstraction_hash,
                       method_url_abstraction_hash]
    hash_collection.sort()
    return hashlib.md5("".join(hash_collection)).hexdigest()


def get_l_http_event_hash_tuple(graph, projname, session, user, logger=None):
    """
    1. select first selenese command
    2. follow caused link
    3. folow the is_followed_by relationship and collect httprequest revents
       - each time also collect abstraction hash
       - save in cons
    4. return cons list
    """
    firstEvent = Event.select(graph).where("_.dm_type='{}'".format(SELENESE)).where("_.projname='{}' AND _.session='{}' AND _.user='{}'".format(projname, session, user)).where("_.seq=1").first()
    #print "{}".format(firstEvent)
    firstHttpRequest = list(firstEvent.Caused)[0]
    #print "{}".format(firstHttpRequest)
    retList = [[firstHttpRequest, get_http_abstraction_hash(firstHttpRequest,
                                                            graph,
                                                            logger)]]
    current = retList[-1][0]
    while list(current.IsFollowedBy) != []:
        current = list(current.IsFollowedBy)[0]
        if current.dm_type == "HttpResponse":
            pass
        else:
            follower = current
            retList.append([follower,
                            get_http_abstraction_hash(follower,
                                                      graph,
                                                      logger)])
            
    return retList


def create_state_cluster_list(event_hash_list, projname):
    buffer = dict()
    clusterList = list()
    counter = 0
    for cons in event_hash_list:
        if cons[1] in buffer:
            clusterList.append(buffer[cons[1]])
        else:
            buffer[cons[1]] = DFAState(projname, HTTPREQ, counter)
            clusterList.append(buffer[cons[1]])
        counter += 1

    clusterList.append(DFAState(projname,
                                HTTPREQ,
                                counter + 1))  # end state

    return clusterList


def get_hash_to_transition(event_hash_list, projname):
    transition_node = dict()
    for cons in event_hash_list:
        if cons[1] in transition_node:
            transition_node[cons[1]].Accepts.add(cons[0])
        else:
            transition_node[cons[1]] = DFAStateTransition(projname,
                                                          HTTPREQ,
                                                          cons[1])
            transition_node[cons[1]].Accepts.add(cons[0])

    return transition_node


def create_dfa(projname, event_hash_list, logger=None):
    state_to_cluster = create_state_cluster_list(event_hash_list, projname)
    hash_to_transition = get_hash_to_transition(event_hash_list, projname)
        
    state_counter = 0
    start_state = state_to_cluster[state_counter]
    current_state = state_to_cluster[state_counter]

    for cons in event_hash_list:
        print "in state {}".format(current_state.uuid)
        state_counter += 1
        transition_node = hash_to_transition[cons[1]]
        print "using transition {}".format(transition_node)
        current_state.HasTransition.add(transition_node)
        transition_node.To.add(state_to_cluster[state_counter])
        current_state = state_to_cluster[state_counter]

    return start_state


def magic_mike(graph, projname, session, user, logger=None):
    logger.info("Retrieveing HTTP requests and clustering by state-changing operations...")
    l_event_hash_tuple = get_l_http_event_hash_tuple(graph, projname, session, user, logger)
    logger.info("Creating DFA...")
    dfa_start = create_dfa(projname, l_event_hash_tuple)
    graph.push(dfa_start)


def hash_to_state(event_hash_list, clusterList):
    counter = 0
    hash_to_state = dict()
    for cons in event_hash_list:
        counter += 1
        if cons[1] in hash_to_state:
            pass
        else:
            hash_to_state[cons[1]] = clusterList[counter]

    return hash_to_state


def insert_intracausality(graph, projname, session, user, logger=None):

    query = """MATCH (e1:Event {dm_type:"HttpRequest", projname:{projname}, session:{session}, user:{user}})<-[]-(pt1:ParseTree), 
                     (pt1)-[:HAS_CHILD*]->(nt:PTNonTerminalNode)-[:HAS_CHILD]->(ref1:PTTerminalNode {symbol:"referer"}), 
                     (nt)-[:HAS_CHILD]->(url_ref1:ParseTree {dm_type:"URL"}), 
                     dist=shortestPath((e2:Event)-[:IS_FOLLOWED_BY*..30]->(e1)), 
                     (pt1:ParseTree)-[:HAS_CHILD]->(url1:ParseTree {dm_type:"URL"}), 
                     (e2)<-[]-(pt2:ParseTree)-[:HAS_CHILD]->(url2:ParseTree {dm_type:"URL"}) 
               WHERE url2.message = url_ref1.message 
       WITH DISTINCT e1, e2 
              RETURN e1.seq, MAX(e2.seq) AS prev
            ORDER BY e1.seq, prev
    """

    rs = graph.run(query, projname=projname, session=session, user=user)
    rs = list(rs)

    if logger is not None:
        logger.info("Adding Intra-causality Edges {}".format(len(rs)))

    i = 1
    for e in rs:
        e1 = Event.select(graph).where(seq=e["e1.seq"], dm_type=HTTPREQ, projname=projname, session=session, user=user).first()
        e2 = Event.select(graph).where(seq=e["prev"], dm_type=HTTPREQ, projname=projname, session=session, user=user).first()
        
        if logger is not None:
            logger.info("Adding intra-causality {}-[IS_GENERATED_BY]->{} ({}/{})".format(e1.seq, e2.seq, i, len(rs)))

        e1.IsGeneratedBy.add(e2)

        graph.push(e1)
        i+=1


def get_all_sql_queries_for_trace(graph, projname, session, user, logger=None):
    query = """MATCH (pt:ParseTree {dm_type:"SQLQuery"})-[PARSES]->(:Event {dm_type:"Xdebug", projname:{projname}, session:{session}, user:{user}}) 
              RETURN pt"""
    return list(graph.run(query, projname=projname, session=session, user=user))


def create_parse_tree_to_abstraction_dictionary(sql_parse_trees):
    dictionary = dict()
    for spt in sql_parse_trees:
        h = sqlnorm.generate_normalized_query_hash(spt['pt']['message'])
        dictionary.setdefault(h, []).append(spt['pt'])
    return dictionary


def add_abstract_sql_queries_for_session_trace(graph, projname, session, user, logger=None):
    if logger is not None:
        logger.info("Retrieving all SQL queries...")
    sql_parse_trees = get_all_sql_queries_for_trace(graph, projname, session, user, logger)
    if logger is not None:
        logger.info("Abstracting SQL queries...")
    dictionary = create_parse_tree_to_abstraction_dictionary(sql_parse_trees)
    for i, kv in enumerate(dictionary.iteritems()):
        key, value = kv
        if logger is not None:
            logger.info("Adding abstract parse tree ({}/{})".format(i, len(dictionary)))

        apt = AbstractParseTree(projname, ABSQUERY, key)
        for pt in value:
            pt_n = ParseTree.select(graph).where(uuid=pt["uuid"]).first()
            apt.Abstracts.add(pt_n)
        graph.push(apt)