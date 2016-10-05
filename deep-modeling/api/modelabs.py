from datamodel.core import *
from datamodel.selenese import *
from datamodel.http import *
from datamodel.sql import *
import sqlnorm
import hashlib


def getAbstractQueries(graph, logger=None):
    returnTable = dict()
    # al.AbstractQuery.select(graph).where() py2neo is stupid
    # list(graph.data("MATCH (n:AbstractQuery) return n"))
    sqlQueries = list(AbstractQuery.select(graph))
    for abstractQuery in sqlQueries:
        returnTable[abstractQuery.hash] = abstractQuery
    return returnTable


def addAbstractionLayerSqlQueries(graph, logger=None):
    abstractQueryTable = getAbstractQueries(graph, logger)
    sqlqueries = list(SQLQuery.select(graph))

    if logger is not None:
        logger.info("abstracting {} queries".format(len(sqlqueries)))

    counter = 1
    for query in sqlqueries:

        if logger is not None:
            logger.info("abstracting query {}/{}".format(counter,
                                                         len(sqlqueries)))

        hash = sqlnorm.generate_normalized_query_hash(query.ts)

        if hash not in abstractQueryTable:
            abstractQueryNode = AbstractQuery(hash)
            abstractQueryTable[hash] = abstractQueryNode
            graph.push(abstractQueryNode)

        query.ABSTRACTSTO.add(abstractQueryTable[hash])
        graph.push(query)
        counter = counter + 1


def getAbstractHTTPRequests(graph, logger=None):
    requestDict = dict()
    for abstract in AbstractHTTPRequest.select(graph):
        requestDict[abstract.hash] = abstract

    return requestDict


def getFullAbstractionHash(HTTPRequest, logger=None):
    accumulator = list()
    for sqlquery in HTTPRequest.Caused:
        accumulator.append(list(sqlquery.ABSTRACTSTO)[0].hash)

    accumulator.sort()
    return hashlib.md5("".join(accumulator)).hexdigest()


def addAbstractionLayerHTTPRequests(graph, logger=None):
    abstractHTTPRequestTable = getAbstractHTTPRequests(graph, logger)
    httpRequests = list(HTTPRequest.select(graph))

    if logger is not None:
        logger.info("abstracting {} HTTPRequests".format(len(httpRequests)))

    counter = 1

    for HTTPRequest in httpRequests:
        if logger is not None:
            logger.info("abstracting {}/{} HTTPRequest"
                        .format(counter, len(httpRequests)))
        
        hash = getFullAbstractionHash(HTTPRequest)

        if hash not in abstractHTTPRequestTable:
                    abstractHTTPRequest = AbstractHTTPRequest(hash)
                    abstractHTTPRequestTable[hash] = abstractHTTPRequest
                    graph.push(abstractHTTPRequest)

        HTTPRequest.ABSTRACTSTO.add(abstractHTTPRequestTable[hash])
        graph.push(HTTPRequest)
        
        counter += 1
