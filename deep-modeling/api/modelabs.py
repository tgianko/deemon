# This file is part of Deemon.

# Deemon is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# Deemon is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with Deemon.  If not, see <http://www.gnu.org/licenses/>.

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


def get_all_sql_queries_of_old(xdebug_event, graph, logger=None):
    sqlQueries = ParseTree.select(graph).where("_.dm_type='{}'".format(ABSQUERY))
    final_list = list()
    for query in list(sqlQueries):
        if list(query.Parses)[0].uuid == xdebug_event.uuid:
            final_list.append(query)

    return final_list


def get_summary_sql_queries_abstraction_hash(httpRequestEvent, graph, logger=None):
    query = """MATCH (a:AbstractParseTree {dm_type:'SqlQuery'})\
-[]->()-[]->()-[]->(:Event {uuid:{uuid}}) RETURN a"""
    rs = graph.run(query, uuid=httpRequestEvent.uuid)
    query_hash_array = list()
    for abstract_query in list(rs):
        query_hash_array.append(abstract_query['a']['message'])
    return hashlib.md5("".join(query_hash_array)).hexdigest()


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
    firstHttpRequest = None
    for request in list(firstEvent.Caused):
        if request.seq == 1:
            firstHttpRequest = request
            break
    assert(firstHttpRequest is not None)
    retList = [[firstHttpRequest, get_http_abstraction_hash(firstHttpRequest,
                                                            graph,
                                                            logger)]]
    current = retList[-1][0]
    while list(current.IsFollowedBy) != []:
        # either it is not followed or the follower is unique
        assert(len(list(current.IsFollowedBy)) <= 1)
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
                                counter + 1))  # COMMENT: end state

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


def create_dfa(projname, event_hash_list, logger):
    if logger is not None:
        logger.info("creating the dfa...")
    state_to_cluster = create_state_cluster_list(event_hash_list, projname)
    hash_to_transition = get_hash_to_transition(event_hash_list, projname)

    state_counter = 0
    start_state = state_to_cluster[state_counter]
    current_state = state_to_cluster[state_counter]

    for i, cons in enumerate(event_hash_list):
        logger.debug("Processing {} / {}".format(i, len(event_hash_list)))
        logger.debug("In state {}".format(current_state.uuid))
        state_counter += 1
        transition_node = hash_to_transition[cons[1]]
        logger.debug("Using transition {}".format(transition_node))
        current_state.HasTransition.add(transition_node)
        transition_node.To.add(state_to_cluster[state_counter])
        current_state = state_to_cluster[state_counter]

    return start_state


def magic_mike(graph, projname, session, user, logger=None):
    if logger is not None:
        logger.info("Retrieveing HTTP requests and clustering by state-changing operations...")

    l_event_hash_tuple = get_l_http_event_hash_tuple(graph, projname,
                                                     session, user, logger)
    dfa_start = create_dfa(projname, l_event_hash_tuple, logger)
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

    if logger is not None:
        logger.info("Adding Intra-causality Edges {}".format(len(rs)))

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

    i = 1
    for e in rs:
        e1 = Event.select(graph).where(seq=e["e1.seq"], dm_type=HTTPREQ, projname=projname, session=session, user=user).first()
        e2 = Event.select(graph).where(seq=e["prev"], dm_type=HTTPREQ, projname=projname, session=session, user=user).first()

        if logger is not None:
            logger.debug("Adding intra-causality {}-[IS_GENERATED_BY]->{} ({}/{})".format(e1.seq, e2.seq, i, len(rs)))

        e1.IsGeneratedBy.add(e2)

        graph.push(e1)
        i += 1


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
            logger.debug("Adding abstract parse tree ({}/{})".format(i, len(dictionary)))

        apt = AbstractParseTree(projname, ABSQUERY, key)
        for pt in value:
            pt_n = ParseTree.select(graph).where(uuid=pt["uuid"]).first()
            apt.Abstracts.add(pt_n)
        graph.push(apt)
