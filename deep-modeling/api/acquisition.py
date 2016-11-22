from parsers import *
from datamodel.core import *
from dm_types import *
from base64 import b64decode


def insert_selenese(graph, cmdlist, projname,
                             session, user, logger=None):
    if logger is not None:
        logger.info("Importing {} Selenese Commands...".format(len(cmdlist)))
    # just in case, we order by ID.
    cmdlist = sorted(cmdlist, key=lambda cmd: cmd[0])

    # CORRECTION OF SEQUENCE NUMBER
    m = min(cmdlist, key=lambda cmd: cmd[0])

    if m[0] == 0:
        if logger is not None:
            logger.warning("Correcting sequence number of selenese: seq starts from 0 while it should start from 1. To align commands to other events, adding a +1");

        def correct(cmd):
            new_cmd = [cmd[0]+1] + list(cmd[1:])
            
            return tuple(new_cmd)

        cmdlist = map(correct, cmdlist)
    # END OF CORRECTION

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

    # CORRECTION OF SEQUENCE NUMBER
    m = min(idlist, key=lambda cmd: cmd[1])

    if m[1] == 0:
        if logger is not None:
            logger.warning("Correcting sequence number of selenese: seq starts from 0 while it should start from 1. To align commands to other events, adding a +1");

        def correct(cmd):
            new_cmd = [cmd[0]] + [cmd[1]+1] + list(cmd[2:])
            
            return tuple(new_cmd)

        idlist = map(correct, idlist)
    # END OF CORRECTION


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

def insert_xdebug(graph, xdebugs, projname, session, user, logger=None):
    if logger is not None:
        logger.info("Importing {} XDEBUG traces".format(len(xdebugs)))
    xdebugs = sorted(xdebugs,  key=lambda r: r[0])

    prev_evt_xdebug = None
    i = 1
    for rs in xdebugs:
        x_id = rs[0]
        if logger is not None:
            logger.info("Processing XDEBUG trace ID {} ({}/{})"
                        .format(x_id, i, len(xdebugs)))

        evt_xdebug = Event(projname, XDEBUG, session, user, x_id, None, "XDEBUG {} TOO HUGE TO STAY HERE.".format(x_id))
        graph.push(evt_xdebug)

        evt_req = Event.select(graph).where(seq=x_id,
                                            projname=projname,
                                            session=session,
                                            user=user,
                                            dm_type=HTTPREQ).first()
        evt_req.Caused.add(evt_xdebug)
        graph.push(evt_req)

        if prev_evt_xdebug:
            prev_evt_xdebug.IsFollowedBy.add(evt_xdebug)
            graph.push(prev_evt_xdebug)
        
        graph.push(evt_req)

        prev_evt_xdebug = evt_xdebug
        i += 1

def insert_queries(graph, queries, projname, session, user, logger=None):
    if logger is not None:
        logger.info("Importing {} XDEBUG traces and SQL query PTs".format(len(queries)))
    i = 1
    for x_id, q_id, sql in queries:
        if logger is not None:
            logger.info("Processing SQL query ID {}-{} ({}/{})"
                        .format(q_id, x_id, i, len(queries)))
        
        evt_xdebug = Event.select(graph).where(seq=x_id,
                                            projname=projname,
                                            session=session,
                                            user=user,
                                            dm_type=XDEBUG).first()
        pt_sql = parse_sql(sql, q_id, None, projname, session, user)
        pt_sql.Parses.add(evt_xdebug)
        graph.push(pt_sql)
        i += 1

def insert_session_dumps(graph, sessions, projname, session, user, logger=None):
    if logger is not None:
        logger.info("Importing {} sessions and relationships\
 with HTTP requests...".format(len(sessions)))
    
    prev_evt_ses = None
    i = 1
    for evt_id, sessnum in sessions:
        if logger is not None:
            logger.info("Processing session {}/{}"
                        .format(i, len(sessions)))
        
        evt_ses = Event(projname, PHPSESSION, session, user, evt_id, None, "{} PHP sessions".format(sessnum))
        graph.push(evt_ses)

        evt_xdebug = Event.select(graph).where(seq=evt_id,
                                            projname=projname,
                                            session=session,
                                            user=user,
                                            dm_type=XDEBUG).first()
        evt_xdebug.Caused.add(evt_ses)
        graph.push(evt_xdebug)

        if prev_evt_ses:
            prev_evt_ses.IsFollowedBy.add(evt_ses)
            graph.push(prev_evt_ses)

        prev_evt_ses = evt_ses
        i += 1


def insert_sessions(graph, sessions, projname, session, user, logger=None):
    if logger is not None:
        logger.info("Importing {} PHP sessions and relationships\
 with Sesssion Traces requests...".format(len(sessions)))

    i = 1
    for evt_id, ses_id, ses_string in sessions:
        if logger is not None:
            logger.info("Processing session {}/{}"
                        .format(i, len(sessions)))
        
        ses_string = b64decode(ses_string)
        try:
            ses_string = b64decode(ses_string) 
        except:
            logger.info("Second base64 decoding no longer needed")
            pass

        ses_id = ses_id.split("_")[-1] # ses_id can be /tmp/sess_

        #print ses_id, ses_string

        pt_ses = parse_session(projname, ses_id, ses_string)

        evt_ses = Event.select(graph).where(seq=evt_id,
                                            projname=projname,
                                            session=session,
                                            user=user,
                                            dm_type=PHPSESSION).first()
        pt_ses.Parses.add(evt_ses)

        graph.push(pt_ses)
        i = i + 1
