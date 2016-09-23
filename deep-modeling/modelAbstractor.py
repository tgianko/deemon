import neo4jmodel.ApplicationDataLevelSQL as adlsql
import neo4jmodel.BrowserActionLevel as bal
import sqlQueryNormalization as sqlNormalizer
import hashlib


def getAbstractQueries(graph, logger=None):
    returnTable = dict()
    # al.AbstractQuery.select(graph).where() py2neo is stupid
    # list(graph.data("MATCH (n:AbstractQuery) return n"))
    sqlQueries = list(adlsql.AbstractQuery.select(graph))
    for abstractQuery in sqlQueries:
        returnTable[abstractQuery.hash] = abstractQuery
    return returnTable


def addAbstractionLayerSqlQueries(graph, logger=None):
    abstractQueryTable = getAbstractQueries(graph, logger)
    sqlqueries = list(adlsql.SQLQuery.select(graph))

    if logger is not None:
        logger.info("abstracting {} queries".format(len(sqlqueries)))

    counter = 1
    for query in sqlqueries:

        if logger is not None:
            logger.info("abstracting query {}/{}".format(counter,
                                                         len(sqlqueries)))

        hash = sqlNormalizer.generate_normalized_query_hash(query.ts)

        if hash not in abstractQueryTable:
            abstractQueryNode = adlsql.AbstractQuery(hash)
            abstractQueryTable[hash] = abstractQueryNode
            graph.push(abstractQueryNode)

        query.ABSTRACTSTO.add(abstractQueryTable[hash])
        graph.push(query)
        counter = counter + 1


def removeNonStateChangingQueries(graph, logger=None):
    pass


def getAbstractHTTPRequests(graph, logger=None):
    pass


def getFullAbstractionHash(HTTPRequest, logger=None):
    accumulator = list()
    for sqlquery in HTTPRequest.Caused:
        accumulator.add(sqlquery.ABSTRACTSTO.first().hash)

    if logger is not None:
        logger.info("aquired list of query hashes {}".format(accumulator))

    accumulator.sort()
    return hashlib.md5("".join(accumulator)).hexdigest()


def removeSingleAbstractHTTPRequests(graph, logger=None):
    abstractHttpRequests = bal.AbstractHTTPRequest.select(graph)

    for abstractHttpRequest in abstractHttpRequests:
        if len(abstractHttpRequests.ABSTRACTSTO) == 1:
            graph.delete(abstractHttpRequest)


def addAbstractionLayerHTTPRequests(graph, logger=None):
    abstractHTTPRequestTable = getAbstractHTTPRequests(graph, logger)
    httpRequests = list(bal.HTTPRequest.select(graph))

    if logger is not None:
        logger.info("abstracting {} HTTPRequests".format(len(httpRequests)))

    for HTTPRequest in httpRequests:
        hash = getFullAbstractionHash(HTTPRequest)

        if hash not in abstractHTTPRequestTable:
                    abstractHTTPRequest = bal.AbstractHTTPRequest(hash)
                    abstractHTTPRequestTable[hash] = abstractHTTPRequest
                    graph.push(abstractHTTPRequest)

        HTTPRequest.AbstractsTo.add(abstractHTTPRequestTable[hash])
        graph.push(HTTPRequest)

        removeSingleAbstractHTTPRequests(graph)
