from parsers import *
from datamodel.core import *
from dm_types import *


def insert_selenese(graph, cmdlist, projname,
                             session, user, logger=None):
    if logger is not None:
        logger.info("Importing {} Selenese Commands...".format(len(cmdlist)))
    # just in case, we order by ID.
    cmdlist = sorted(cmdlist, key=lambda cmd: cmd[0])

    prev_evt = None
    for cmd in cmdlist:
        if logger is not None:
            logger.info("Adding Selenese Command (PT and Trace) {} / {}".format(cmd[0],
                                                               len(cmdlist)))
        seq     = cmd[0]
        command = cmd[2]
        target  = cmd[3]
        value   = cmd[4]
        
        message = "command={}, target={}, value={}".format(command, target, value)
        evt = Event(projname, SELENESE, session, user, seq, None, message)
        pt  = parse_selcmd(command, target, value, seq,
                                    None, projname, session, user)
        pt.Parses.add(evt)

        graph.push(pt)

        if prev_evt:
            prev_evt.IsFollowedBy.add(evt)
            graph.push(prev_evt)
        
        prev_evt = evt  

 
def insert_http(graph, reqlist, resplist, projname, session, user, logger=None):
    if logger is not None:
        logger.info("Importing {} HTTP messages...".format(len(reqlist)))
    # just in case, we order by ID.
    reqlist = sorted(reqlist,  key=lambda r: r[0])
    resplist = sorted(resplist, key=lambda r: r[0])
    
    prev_evt_res = None
    i = 1
    for hreq, hres in zip(reqlist, resplist):
        seq     = hreq[0]
        logger.info("Adding HTTP Messages (PT and Trace) {} ({}/{})".format(hreq[0], i,
                                                                len(reqlist)))
        message = "{} {}".format(hreq[6], hreq[3])
        evt_req = Event(projname, HTTPREQ, session, user, hreq[0], hreq[2], message)
        pt_req = parse_httpreq(hreq[6], hreq[3], hreq[5],
                                     hreq[4], hreq[0], hreq[2],
                                      projname, session, user)
        pt_req.Parses.add(evt_req)

        graph.push(evt_req)
        graph.push(pt_req)

        message = "{}".format(hres[3])
        evt_res = Event(projname, HTTPRESP, session, user, hres[0], hres[2], message)
        pt_res = parse_httpres(hres[3], hres[4], hres[5],
                                      hres[0], hres[2], projname,
                                      session, user)
        pt_res.Parses.add(evt_res)
        evt_req.IsFollowedBy.add(evt_res)

        graph.push(evt_res)
        graph.push(pt_res)

        if prev_evt_res:
            prev_evt_res.IsFollowedBy.add(evt_req)
            graph.push(prev_evt_res)
        
        graph.push(evt_req)

        prev_evt_res = evt_res
        i += 2




def insert_causality_selhttp(graph, idlist, projname, session, user, logger=None):
    if logger is not None:
        logger.info("Importing {} causality between Selenese\
 commands and HTTP requests...".format(len(idlist)))
    i = 1
    for rid, cmdid in idlist:
        if logger is not None:
            logger.info("Processing Selense command ID {} ->\
 HTTP request ID {} ({}/{})".format(cmdid, rid, i, len(idlist)))
        evt_cmd = Event.select(graph).where(seq=cmdid,
                                          projname=projname,
                                          session=session,
                                          user=user,
                                          dm_type=SELENESE).first()

        evt_hreq = Event.select(graph).where(seq=rid,
                                                 projname=projname,
                                                 session=session,
                                                 user=user,
                                                 dm_type=HTTPREQ).first()
        evt_cmd.Caused.add(evt_hreq)
        graph.push(evt_cmd)
        i += 1


def insert_queries(graph, queries, projname, session, user, logger=None):
    if logger is not None:
        logger.info("Importing {} SQL queries and relationships\
 with HTTP requests...".format(len(queries)))
    i = 1
    for hreq_id, q_id, sql in queries:
        if logger is not None:
            logger.info("Processing SQL query ID {}-{} ({}/{})"
                        .format(q_id, hreq_id, i, len(queries)))
        n_id = "{}.{}".format(hreq_id, q_id)
        sql_n = parse_sql(sql, n_id, None, projname, session, user)
        hreq_n = HTTPRequest.select(graph).where(seq=hreq_id,
                                                     projname=projname,
                                                     session=session,
                                                     user=user).first()
        hreq_n.Causes.add(sql_n)
        graph.push(hreq_n)
        graph.push(sql_n)
        i += 1


def insert_sessions(graph, sessions, projname, session, user, logger=None):
    if logger is not None:
        logger.info("Importing {} sessions and relationships\
 with HTTP requests...".format(len(sessions)))
    
    counter = 1

    for hreq_id, ses_string in sessions:
        if logger is not None:
            logger.info("Processing session {}/{}"
                        .format(counter, len(sessions)))
        
        sess = sessionparser.parseSession(ses_string)
        hreq_n = bal.HTTPRequest.select(graph).where(seq=hreq_id,
                                                     projname=projname,
                                                     session=session,
                                                     user=user).first()
        graph.push(sess)
        hreq_n.Caused.add(sess)
        graph.push(hreq_n)
        counter = counter + 1
