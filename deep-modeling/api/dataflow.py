from datamodel.core import *
from dm_types import *

def insert_propagates_to(graph, rs, logger):

    rs = list(rs)
    i = 1
    for e in rs:
        if logger is not None:
            logger.info("Inserting relationship {}/{}".format(i, len(rs)))
        var1 =  Variable.select(graph).where(uuid=e[0]).first()
        var2 =  Variable.select(graph).where(uuid=e[1]).first()
        
        var1.PropagatesTo.add(var2)

        graph.push(var1)
        i+=1

def insert_backward_selenese_chains(graph, projname, session, user, logger):
    if logger is not None:
        logger.info("Adding Cross-Tier, Causal, Propagation Chains...")

    query = """MATCH (v1:Variable)-[:BELONGS_TO]->(e_sel1:Event {dm_type:"SeleneseCommand", projname: {projname}, session:{session}, user:{user}})-[:IS_FOLLOWED_BY*]->(e_sel2:Event {dm_type:"SeleneseCommand"})-[:CAUSED]->(e_http1:Event {dm_type:"HttpRequest"})<-[:BELONGS_TO]-(v2:Variable) 
                WITH v1, v2
               WHERE v1.value=v2.value
               RETURN v1.uuid,
                      v2.uuid"""

    rs = graph.run(query, projname=projname, session=session, user=user)
    
    insert_propagates_to(graph, rs, logger)



def insert_vertical_chains(graph, projname, session, user, logger):
    if logger is not None:
        logger.info("Adding Cross-Tier, Causal, Propagation Chains...")

    query = """MATCH (v_http:Variable)-[:BELONGS_TO]->(e_http:Event {projname: {projname}, session:{session}, user:{user}})-[:CAUSED*]->(e_final:Event)<-[:BELONGS_TO]-(v_final:Variable) 
                WITH v_http, v_final
               WHERE v_http.value=v_final.value 
              RETURN v_http.uuid, 
                     v_final.uuid"""
    
    rs = graph.run(query, projname=projname, session=session, user=user)
    
    insert_propagates_to(graph, rs, logger)

def insert_variables(graph, projname, session, user, logger):

    BLACKLIST = ["command-name", "command-value", 
                 "target-name", "target-value",
                 "value-name",
                 "HTTP-version",
                 "method",
                 "scheme",
                 "field-name",
                 "plaintext-body",
                 "Token.Operator.Comparison", "Token.Operator", 
                 "Token.Keyword", "Token.Keyword.DML",
                 "Token.Name.Builtin"]

    if logger is not None:
        logger.info("Deriving Variables from PTs...")
    
    query = """MATCH p1=(pt:ParseTree)-[:HAS_CHILD*]->(d:PTTerminalNode), 
                     p2=(pt)-[par:PARSES]->(e:Event {projname: {projname}, session:{session}, user:{user}}) 
               WHERE size(d.symbol) > 0 
              UNWIND nodes(p1) AS t 
                WITH collect(t) AS sli, 
                     d, 
                     pt, 
                     e 
              RETURN e.dm_type AS dm_type,
                     e.seq AS seq,
                     reduce(s="(" + e.seq + ")" + e.dm_type, t IN sli | s + ">" + "(" + t.pos + ")" + coalesce(t.s_type, t.dm_type)) AS name, 
                     d.symbol AS value,
                     d.uuid AS has_value_uuid,
                     e.uuid AS belongs_to_uuid,
                     d.s_type as s_type"""
    rs = graph.run(query, projname=projname, session=session, user=user)
    rs = list(rs)
    
    def do_blacklist(e):
        return e["s_type"] not in BLACKLIST

    if logger is not None:
        logger.info("Applying blacklist of s_type")
    rs = filter(do_blacklist, rs)


    i = 1
    for e in rs:
        if e["s_type"] in BLACKLIST:
            if logger is not None:
                logger.info("Ignoring s_type {} {}/{}".format(e["s_type"], i, len(rs)))
            continue
        if logger is not None:
            logger.info("Inserting variable {}/{}".format(i, len(rs)))
        var =  Variable(projname, e["dm_type"], session, user, e["seq"], e["name"], e["value"])
        
        value = PTTerminalNode.select(graph).where(uuid=e["has_value_uuid"]).first()
        var.HasValue.add(value)

        event = Event.select(graph).where(uuid=e["belongs_to_uuid"]).first()
        var.BelongsTo.add(event)

        graph.push(var)
        i+=1


