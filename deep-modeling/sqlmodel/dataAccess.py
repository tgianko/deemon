import sqlite3 as lite


def load_selcmd_sqlite(fname, logger=None):
    if logger is not None:
        logger.info("Loading Selense commands from vilanoo2/mosgi SQLite db")

    con = lite.connect(fname)
    cmdlist = []
    with con:
        cur = con.cursor()
        rs = cur.execute("SELECT * FROM selenese_commands ORDER BY id")
        cmdlist = list(rs)
    return cmdlist

    
def load_hreqs_sqlite(fname, logger=None):
    if logger is not None:
        logger.info("Loading HTTP requests from vilanoo2/mosgi SQLite db")

    con = lite.connect(fname)
    reqlist = []
    with con:
        cur = con.cursor()
        rs = cur.execute("SELECT * FROM http_requests ORDER BY id")
        reqlist = list(rs)
    return reqlist


def load_hres_sqlite(fname, logger=None):
    if logger is not None:
        logger.info("Loading HTTP responses from vilanoo2/mosgi SQLite db")

    con = lite.connect(fname)
    resplist = []
    with con:
        cur = con.cursor()
        rs = cur.execute("SELECT * FROM http_responses ORDER BY id")
        resplist = list(rs)
    return resplist


def load_cmd2http_sqlite(fname, logger=None):
    if logger is not None:
        logger.info("Loading Selenese command to HTTP requests\
 relationships from vilanoo2/mosgi SQLite db")

    con = lite.connect(fname)
    ids = []
    with con:
        cur = con.cursor()
        rs = cur.execute("SELECT id, command_id FROM http_requests")
        ids = list(rs)
    return ids


def load_queries_sqlite(fname, logger=None):
    if logger is not None:
        logger.info("Loading SQL queries from  Analyzer SQLite db")

    con = lite.connect(fname)
    ids = []
    with con:
        cur = con.cursor()
        rs = cur.execute("SELECT * FROM sql_queries")
        ids = list(rs)
    return ids


def load_php_sessions(fname, logger=None):
    if logger is not None:
        logger.info("Loading SQL queries from Analyzer SQLite db")

    con = lite.connect(fname)
    ids = []
    with con:
        cur = con.cursor()
        rs = cur.execute("SELECT http_request_id,session_string FROM sessions")
        ids = list(rs)
    return ids
