from parsers import *
from datamodel.core import *
from datamodel.selenese import *
from datamodel.http import *
from datamodel.sql import *


def insert_selenese_commands(graph, cmdlist, projname,
                             session, user, logger=None):
    if logger is not None:
        logger.info("Importing {} commands...".format(len(cmdlist)))
    # just in case, we order by ID.
    cmdlist = sorted(cmdlist, key=lambda cmd: cmd[0])

    prev_cmd_n = None
    for cmd in cmdlist:
        if logger is not None:
            logger.info("Processing Command ID {} / {}".format(cmd[0],
                                                               len(cmdlist)))
        cmd_n = parse_selcmd(cmd[2], cmd[3], cmd[4], cmd[0],
                                    None, projname, session, user)

        if prev_cmd_n:
            prev_cmd_n.Next.add(cmd_n)
            graph.push(prev_cmd_n)
        
        graph.push(cmd_n)

        prev_cmd_n = cmd_n


def insert_httpreqs(graph, reqlist, projname, session, user, logger=None):
    if logger is not None:
        logger.info("Importing {} HTTP requests...".format(len(reqlist)))
    # just in case, we order by ID.
    reqlist = sorted(reqlist, key=lambda r: r[0])
    
    prev_hreq_n = None
    i = 1
    for hreq in reqlist:
        logger.info("Processing HTTP Request ID {} ({}/{})".format(hreq[0], i,
                                                                len(reqlist)))
        hreq_n = parse_httpreq(hreq[6], hreq[3], hreq[5],
                                     hreq[4], hreq[0], hreq[2],
                                      projname, session, user)
        if prev_hreq_n:
            prev_hreq_n.Next.add(hreq_n)
            graph.push(prev_hreq_n)
        
        graph.push(hreq_n)
        prev_hreq_n = hreq_n
        i += 1


def insert_httpresps(graph, resplist, projname, session, user, logger=None):
    if logger is not None:
        logger.info("Importing {} HTTP responses and relationships\
 with HTTP requests...".format(len(resplist)))
    # just in case, we order by ID.
    resplist = sorted(resplist, key=lambda r: r[0])
    i = 1
    for hres in resplist:
        if logger is not None:
            logger.info("Processing HTTP Response\
 ID {} ({}/{})".format(hres[0], i, len(resplist)))
        hres_n = parse_httpres(hres[3], hres[4], hres[5],
                                      hres[0], hres[2], projname,
                                      session, user)
        
        hreq_n = HTTPRequest.select(graph).where(seq=hres[1],
                                                     projname=projname,
                                                     session=session,
                                                     user=user).first()
        hreq_n.Transaction.add(hres_n)
        graph.push(hreq_n)
        graph.push(hres_n)
        i += 1


def insert_cmd2http(graph, idlist, projname, session, user, logger=None):
    if logger is not None:
        logger.info("Importing {} relationships between Selenese\
 commands and HTTP requests...".format(len(idlist)))
    i = 1
    for rid, cmdid in idlist:
        if logger is not None:
            logger.info("Processing Selense command ID {} ->\
 HTTP request ID {} ({}/{})".format(cmdid, rid, i, len(idlist)))
        cmd_n = SeleneseCommand.select(graph).where(seq=cmdid,
                                                        projname=projname,
                                                        session=session,
                                                        user=user).first()
        hreq_n = HTTPRequest.select(graph).where(seq=rid,
                                                     projname=projname,
                                                     session=session,
                                                     user=user).first()
        cmd_n.Causes.add(hreq_n)
        graph.push(cmd_n)
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
