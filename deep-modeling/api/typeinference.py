from datamodel.core import Variable, Event, AbstractEvent
from dm_types import *
import typeinfalg.typeinference as typeinf
from urlparse import urlparse, urlunparse, parse_qs
from urllib import urlencode

from pprint import pprint

def insert_user_generated_chains(graph, projname, session, user, logger):
    
    query = """MATCH chain=(src:Variable)-[:PROPAGATES_TO*]->(dst:Variable), 
                     constr=(src {session: {session}, projname:{projname}, user:{user}})-[:HAS_VALUE]->(tn:PTTerminalNode {s_type:"value-value", dm_type:"SeleneseCommand"}) 
                WITH nodes(chain) AS vlist, src, dst UNWIND vlist AS v 
                WITH DISTINCT v 
              RETURN v.uuid
    """

    rs = graph.run(query, projname=projname, session=session, user=user)
    rs = list(rs)

    if logger is not None:
        logger.info("Inferring user generated propagation chains {}".format(len(rs)))

    i = 1
    for e in rs:
        var = Variable.select(graph).where(uuid=e["v.uuid"]).first()
        
        if var.proptype is None:
            var.proptype = list()

        if UG not in var.proptype:
            var.proptype.append(UG)
            if logger is not None:
                logger.info("Adding {} type to variable ({}/{})".format(UG, i, len(rs)))
        else:
            logger.info("Skipping {} type. Already in variable ({}/{})".format(UG, i, len(rs)))

        graph.push(var)
        i+=1

def _do_infer(graph, var_groups, logger):
    # inference

    for j, v_g in enumerate(var_groups):

        syninf_pars = [v["value"] for v in v_g]
        syn_type = str(typeinf.infer_syntactic_type(syninf_pars))

        seminf_pars = [{"user": v["user"], "value":v["value"]} for v in v_g]
        sem_type = str(typeinf.infer_semantic_type(seminf_pars))

        # n = ">".join(v_g[0]["name"].split(">")[1:])
        # print "{:100} {:10} {:10}    values={}".format(n, syn_type, sem_type, seminf_pars)

        for i, v in enumerate(v_g):
            v_node = Variable.select(graph).where(uuid=v["uuid"]).first()

            if v_node.syntype is None:
                v_node.syntype = list()

            if syn_type not in v_node.syntype:
                v_node.syntype.append(syn_type)
                if logger is not None:
                    logger.info("Adding SYN {} type to variable ({}/{} {}/{})".format(syn_type, i, len(v_g), j, len(var_groups)))
            else:
                logger.info("Skipping SYN {} type. Already in variable ({}/{} {}/{})".format(syn_type, i, len(v_g), j, len(var_groups)))


            if v_node.semtype is None:
                v_node.semtype = list()
            
            if sem_type not in v_node.semtype:
                v_node.semtype.append(sem_type)
                if logger is not None:
                    logger.info("Adding SEM {} type to variable ({}/{} {}/{})".format(sem_type, i, len(v_g), j, len(var_groups)))
            else:
                logger.info("Skipping SEM {} type. Already in variable ({}/{} {}/{})".format(sem_type, i, len(v_g), j, len(var_groups)))

            graph.push(v_node)


# def insert_synsem_type(graph, logger):
#     if logger is not None:
#         logger.info("Adding Semantic Types...")

#     query = """MATCH ()<-[:IS_GENERATED_BY]-(n:Event {dm_type:"HttpRequest"})<-[:IS_GENERATED_BY]-(), (n)<-[:BELONGS_TO]-(v:Variable) 
#                 WITH DISTINCT n, v 
#               RETURN n, collect(v) as variables"""

#     rs = graph.run(query)
    
#     var_groups = _groups_vars_by_vname(rs)

#     # normalized: when sent to the STDOUT it looks much better
#     var_groups = _sort_groups(var_groups)

#     _do_infer(graph, var_groups, logger)


#     query = """MATCH (n:Event {dm_type:"HttpRequest"})<-[:BELONGS_TO]-(v:Variable) 
#                 WITH DISTINCT n, v 
#                WHERE NOT (n)<-[:IS_GENERATED_BY]-() 
#               RETURN n, collect(v) as variables"""

#     rs = graph.run(query)
    
#     var_groups = _groups_vars_by_vname(rs)

#     # normalized: when sent to the STDOUT it looks much better
#     var_groups = _sort_groups(var_groups)

#     _do_infer(graph, var_groups, logger)    



def get_users_sessions_from_operation(graph, operation, projname):
    query = """MATCH (e:Event {projname:{projname}})
                WITH DISTINCT e.user AS user, e.session AS session
               WHERE left(session, size(session)-3) = {operation}
              RETURN user, session"""    

    rs = graph.run(query, projname=projname, operation=operation)
    return list(rs)

def _abs_url(u):
    scheme, netloc, path, params, query, fragment = urlparse(u)
    query_p = parse_qs (query)

    """ remove qs values """
    for k in query_p:
        query_p[k] = ""

    query = urlencode(query_p)

    new_u = urlunparse((scheme, netloc, path, params, query, fragment))
    #print u, new_u
    return new_u

def _equal(r1, r2):
    if None in [r1, r2]:
        return True

    m1 = r1.message.split(" ")[0]
    m2 = r2.message.split(" ")[0]
    if m1 != m2:
        return False

    u1 = r1.message.split(" ")[1]
    u2 = r2.message.split(" ")[1]
    #print _abs_url(u1), _abs_url(u2), _abs_url(u1) == _abs_url(u2)
    return _abs_url(u1) == _abs_url(u2)

def are_all_equal(*reqs):
    iterator = iter(reqs)
    try:
        first = next(iterator)
    except StopIteration:
        return True
    return all(_equal(first, rest) for rest in iterator)


def _index(l, obj):
    for index, item in enumerate(l):
        if _equal(item, obj):
            break
    else:
        index = -1
    return index

def align_sequences(seqs, graph, logger):
    seq1, seq2 = seqs

    # for e1, e2 in map(None, seq1, seq2):
    #     m1 = None
    #     s1 = -1
    #     if e1:
    #         m1 = e1.message
    #         s1 = e1.seq

    #     m2 = None
    #     s2 = -1
    #     if e2:
    #         m2 = e2.message
    #         s2 = e2.seq

    #     print "{:3} {:80} | {:3} {:80} ".format(s1, m1, s2, m2)

    #     # logger.info("BEFORE>=====<BEFORE")
    #     # logger.info(m1)
    #     # logger.info(m2)
    #     # logger.info("BEFORE>=====<BEFORE")

    maxlen = len(seq1)+len(seq2)
    logger.info("Len(seq1)={}, Len(seq2)={}".format(len(seq1), len(seq2)))
    for i in range(0, maxlen):
        
        if i >= len(seq1) or i >= len(seq2):
            break

        if None in [seq1[i], seq2[i]]:
            logger.info("One is none")
            continue
        elif _equal(seq1[i], seq2[i]):
            logger.info("Equal")
            logger.info(" = {}".format(seq1[i].message))
            logger.info(" = {}".format(seq2[i].message))
            continue
        else:
            logger.info("Different. Need to shift.")
            logger.info(" ? {}".format(seq1[i].message))
            logger.info(" ? {}".format(seq2[i].message))


            # how far is the first element in seq2 equal to seq1[i]?
            pos1 = _index(seq2[i+1:], seq1[i])
            # how far is the first element in seq1 equal to seq2[i]?
            pos2 = _index(seq1[i+1:], seq2[i])

            
            logger.info("  - Distances: pos1={} pos2={}".format(pos1, pos2))

            
            if pos1 >= 0 and pos2 >= 0:
                """
                Both list have one, but we pick the min
                """
                if pos2 > pos1: # pos1 is closer => shift S2

                    logger.info("    > pos1 is closer => Shift seq2 of {}".format(pos1+1))
                    seq2 = seq2[0:i] + [None]*(pos1+1) + seq2[i:]

                elif pos2 < pos1: # pos2 is closer => shift S1

                    logger.info("    > pos2 is closer => Shift seq1 of {}".format(pos2+1))
                    seq1 = seq1[0:i] + [None]*(pos2+1) + seq1[i:]

                elif pos1 in range(0, 2): # they are at the same distance (we use a tollerance of 1, i.e., 0, 1)

                    logger.info("    > pos1 and pos2 are inverted and very close => swap seq1")
                    seq1[i], seq1[i+pos1+1] = seq1[i+pos1+1], seq1[i]

                else: # they are too far, we shift ahead 

                    logger.info("    > pos1 and pos2 are inverted but distant => Shift seq1")
                    seq1 = seq1[0:i] + [None]*(pos2+1) + seq1[i:]

            elif pos1 < 0 and pos2 < 0: # both do not exist in the remainig on the sequences. We misalign both and move on.

                logger.info("    > pos1 and pos2 are both null => misalign both of 1")
                seq1 = seq1[0:i] + [None] + seq1[i:]
                seq2 = seq2[0:i+1] + [None] + seq1[i+1:]
                
            elif pos1 < 0: # we shift the non null one

                logger.info("    > only pos1 is null => shift pos2 of {}".format(pos2+1))
                seq2 = seq2[0:i] + [None]*(pos2+1) + seq2[i:]

            elif pos2 < 0: # we shift the non null one

                logger.info("    > only pos2 is null => shift pos1 of {}".format(pos1+1))
                seq1 = seq1[0:i] + [None]*(pos1+1) + seq1[i:]

            else:
                raise Exception("I should have not been reached sort-of exception when aligning sequences: pos1={}, pos2={}".format(pos1, pos2))
    """
    At this point len(seq1) <> len(seq2) ... we adjust it with a little trick:
    """
    n_seq1 = []
    n_seq2 = []
    for e1, e2 in map(None, seq1, seq2):
        if e1 is None and e2 is None:
            continue

        n_seq1.append(e1)
        n_seq2.append(e2)

    seq1 = n_seq1
    seq2 = n_seq2

    logger.info("Len(seq1)={}, Len(seq2)={}".format(len(seq1), len(seq2)))

    # for e1, e2 in zip(seq1, seq2):
    #     m1 = None
    #     s1 = -1
    #     if e1:
    #         m1 = e1.message
    #         s1 = e1.seq

    #     m2 = None
    #     s2 = -1
    #     if e2:
    #         m2 = e2.message
    #         s2 = e2.seq

    #     logger.info("AFTER>=====<AFTER")
    #     logger.info("{:3} {}".format(s1, m1))
    #     logger.info("{:3} {}".format(s2, m2))
    #     logger.info("-----------------")

    return [seq1, seq2]


def _get_and_group_vars(sequence, graph, logger):
    var_groups = []
    for pos, abs_evt in enumerate(zip(*sequence)):
        print abs_evt
        uuid = abs_evt["uuid"]
        query = """MATCH (ae:AbstractEvent {uuid:{uuid}})-[:ABSTRACTS]->(e:Event)<-[:BELONGS_TO]-(v:Variable)
                  RETURN DISTINCT v"""

        rs = graph.run(query, uuids=uuids)
        req_groups = {}
        for el in list(rs):
            v = el["v"]
            if v:
                #var_name = ">".join(v["name"].split(">")[1:])
                var_name = v["name"]
                req_groups.setdefault(var_name, []).append(v)
            else:
                logger.info("{} element".format(v))
        var_groups.extend(req_groups.itervalues())

    return var_groups


def _get_aligned_sequences(graph, logger, operation, projname):
    us_se_pairs = get_users_sessions_from_operation(graph, operation, projname)

    seqs_S1 = []
    seqs_S2 = []
    for us, se in us_se_pairs:
        seq = list(Event.select(graph).where(dm_type=HTTPREQ, user=us, session=se, projname=projname))
        seq = sorted(seq, key=lambda r: r.seq)
        if se.endswith("S1"):
            seqs_S1.append(seq)
        elif se.endswith("S2"):
            seqs_S2.append(seq)
        else:
            logger.warning("Unknown session {}".format(se))

    logger.info("Length of sequences: S1={} S1={}".format(len(seqs_S1), len(seqs_S2)))
    sequences = []
    if len(seqs_S1) > 1 and len(seqs_S2) > 1:
        seqs_S1 = align_sequences(seqs_S1, graph, logger)
        seqs_S2 = align_sequences(seqs_S2, graph, logger)
        sequences = seqs_S1 + seqs_S2
    elif len(seqs_S1) == 1 and len(seqs_S2) == 1:
        sequences = align_sequences(seqs_S1 + seqs_S2, graph, logger)
    else:
        logger.warning("Could not align sequences projname {} operation {}".format(projname, operation))
        raise Exception("Could not align sequences projname {} operation {}".format(projname, operation))
    
    return sequences


def insert_abstract_events(graph, logger, operation, projname):
    if logger is not None:
        logger.info("Aligning Events and Adding Abstract Events ...")

    sequences = _get_aligned_sequences(graph, logger, operation, projname)

    prev_abs_evt = None
    reqs_by_pos = zip(*sequences)
    for pos, reqs in enumerate(reqs_by_pos):
        pos+=1
        a_req = next(req for req in reqs if req is not None) # a request not null
        abs_evt = AbstractEvent(projname, ABSHTTPREQ, operation, pos, _abs_url(a_req.message))
        if logger is not None:
            logger.info("Adding abstract event of position {}/{}".format(pos, len(reqs_by_pos)))
        
        for req in reqs:
            if req:
                # if logger is not None:
                #     logger.info("{} abstracts {}".format(abs_evt.uuid, req.uuid))
                abs_evt.Abstracts.add(req)

        if prev_abs_evt:
            prev_abs_evt.IsFollowedBy.add(abs_evt)
            graph.push(prev_abs_evt)

        graph.push(abs_evt)

        prev_abs_evt = abs_evt


def insert_synsem_type_by_op(graph, logger, operation, projname):
    if logger is not None:
        logger.info("Adding Semantic Types...")

    query = """MATCH (e:AbstractEvent {projname:{projname}, operation:{operation}})
              RETURN DISTINCT e"""

    rs = list(graph.run(query, projname=projname, operation=operation))

    if logger is not None:
        logger.info("Grouping variables by AbstractEvent...")
    var_groups = []
    for pos, el in enumerate(rs):
        uuid = el["e"]["uuid"]
        query = """MATCH (ae:AbstractEvent {uuid:{uuid}})-[:ABSTRACTS]->(e:Event)<-[:BELONGS_TO]-(v:Variable)
                  RETURN DISTINCT v"""

        rs = graph.run(query, uuid=uuid)
        req_groups = {}
        for el in list(rs):
            v = el["v"]
            if v:
                #var_name = ">".join(v["name"].split(">")[1:])
                var_name = v["name"]
                req_groups.setdefault(var_name, []).append(v)
            else:
                logger.info("{} element".format(v))
        var_groups.extend(req_groups.itervalues())

    # for vs in var_groups:
    #     print "{:10} | {:80} | {}".format(len(vs), vs[0]["name"], [v["value"] for v in vs])

    _do_infer(graph, var_groups, logger) 