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

        hash = sqlNormalizer.generate_normalized_query_hash(query.sql)

        if hash not in abstractQueryTable:
            abstractQueryNode = adlsql.AbstractQuery(hash)
            abstractQueryTable[hash] = abstractQueryNode
            graph.push(abstractQueryNode)

        query.ABSTRACTSTO.add(abstractQueryTable[hash])
        graph.push(query)
        counter = counter + 1


def getAbstractHTTPRequests(graph, logger=None):
    requestDict = dict()
    for abstract in bal.AbstractHTTPRequest.select(graph):
        requestDict[abstract.hash] = abstract

    return requestDict


def getFullAbstractionHash(HTTPRequest, logger=None):
    accumulator = list()

    for relation in HTTPRequest.Caused:
        if isinstance(relation, adlsql.SQLQuery):
            abstraction = list(relation.ABSTRACTSTO)[0]
            accumulator.append(abstraction.hash)

    accumulator.sort()
    return hashlib.md5("".join(accumulator)).hexdigest()


def addAbstractionLayerHTTPRequests(graph, logger=None):
    abstractHTTPRequestTable = getAbstractHTTPRequests(graph, logger)
    httpRequests = list(bal.HTTPRequest.select(graph))

    if logger is not None:
        logger.info("abstracting {} HTTPRequests".format(len(httpRequests)))

    counter = 1

    for HTTPRequest in httpRequests:
        if logger is not None:
            logger.info("abstracting {}/{} HTTPRequest"
                        .format(counter, len(httpRequests)))
        
        hash = getFullAbstractionHash(HTTPRequest)

        if hash not in abstractHTTPRequestTable:
                    abstractHTTPRequest = bal.AbstractHTTPRequest(hash)
                    abstractHTTPRequestTable[hash] = abstractHTTPRequest
                    graph.push(abstractHTTPRequest)

        HTTPRequest.ABSTRACTSTO.add(abstractHTTPRequestTable[hash])
        graph.push(HTTPRequest)
        
        counter += 1


def add_full_abstraction_layer(graph, logger=None):
    addAbstractionLayerSqlQueries(graph, logger)
    addAbstractionLayerHTTPRequests(graph, logger)


def get_HTTPRequests(graph, logger=None):
    return map(lambda x: x[0],
               graph.run("MATCH (h:HTTPRequest) RETURN h"))
