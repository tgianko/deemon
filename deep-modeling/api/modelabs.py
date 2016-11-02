from datamodel.core import Event, ParseTree, DFAState, DFAStateTransition
import sqlnorm
import hashlib

seleneseIdent = "SeleneseCommand"
httpRequestIdent = "HttpRequest"
SQLQueryIdent = "SQLQuery"


def get_all_sql_queries_of(xdebug_event, graph, logger=None):
    sqlQueries = ParseTree.select(graph).where("_.dm_type='{}'"
                                               .format(SQLQueryIdent))
    final_list = list()
    for query in list(sqlQueries):
        if list(query.Parses)[0].uuid == xdebug_event.uuid:
            final_list.append(query)

    return final_list


def get_http_abstraction_hash(httpRequestEvent, graph, logger=None):
    if list(httpRequestEvent.Caused) == []:
        print "{} has no xdebug".format(httpRequestEvent)
        return hashlib.md5("").hexdigest()
    else:
        xdebug = list(httpRequestEvent.Caused)[0]
        query_hashes = list()
        for queryParseTree in get_all_sql_queries_of(xdebug, graph, logger):
            query_hashes.append(sqlnorm.generate_normalized_query_hash(queryParseTree.message))

            query_hashes.sort()
        return hashlib.md5("".join(query_hashes)).hexdigest()


def get_l_http_event_hash_tuple(projname, graph, logger=None):
    """
    1. select first selenese command
    2. follow caused link
    3. folow the is_followed_by relationship and collect httprequest revents
       - each time also collect abstraction hash
       - save in cons
    4. return cons list
    """
    firstEvent = Event.select(graph).where("_.dm_type='{}'".format(seleneseIdent)).where("_.projname='{}'".format(projname)).where("_.seq=1").first()
    print "{}".format(firstEvent)
    firstHttpRequest = list(firstEvent.Caused)[0]
    print "{}".format(firstHttpRequest)
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
            buffer[cons[1]] = DFAState(projname, httpRequestIdent, counter)
            clusterList.append(buffer[cons[1]])

    clusterList.append(DFAState(projname,
                                httpRequestIdent,
                                counter + 1))  # end state

    return clusterList


def get_hash_to_transition(event_hash_list, projname):
    transition_node = dict()
    for cons in event_hash_list:
        if cons[1] in transition_node:
            pass
        else:
            transition_node[cons[1]] = DFAStateTransition(projname,
                                                          httpRequestIdent,
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


def magic_mike(projname, graph, logger=None):
    l_event_hash_tuple = get_l_http_event_hash_tuple(projname, graph, logger)
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
