from py2neo.ogm import GraphObject, Property, RelatedTo
from uuid import uuid4
from GenericElements import KeyValuePair

class SQLToken(GraphObject):

    __primarykey__ = "uuid"

    uuid  = Property()

    ttype = Property()
    value = Property()

    def __init__(self, projname=None, ttype=None, value=None, tags=[]):
        self.projname = projname
        self.ttype    = ttype
        self.value    = value
        self.tags     = tags
        self.uuid = str(uuid4())


class SQLTokenList(GraphObject):

    __primarykey__ = "uuid"

    uuid  = Property()

    Child = RelatedTo("SQLToken")

    def __init__(self, projname=None):
        self.projname = projname
        self.uuid = str(uuid4())


class SQLStatement(GraphObject):

    __primarykey__ = "uuid"

    uuid  = Property()

    ttype     = Property()
    stmt      = Property()  

    Child = RelatedTo("SQLToken") 

    def __init__(self, projname=None, statement=None):
        self.projname  = projname
        self.stmt = statement
        self.uuid = str(uuid4())


class QueryString(GraphObject):

    __primarykey__ = "uuid"
    
    uuid  = Property()

    projname = Property()

    qs = Property()

    Parameter = RelatedTo("KeyValuePair")

    def __init__(self, projname=None, qs=None):
        self.projname=projname
        self.qs = qs
        self.uuid  = str(uuid4())


class SQLQuery(GraphObject):
    
    __primarykey__ = "uuid"

    uuid      = Property()

    projname  = Property()
    
    session   = Property()
    user      = Property()
    seq       = Property()
    ts        = Property()
    
    sql       = Property()

    Statement = RelatedTo("SQLStatement")

    def __init__(self, projname=None, session=None, user=None, seq=None, ts=None, sql=None):
        self.projname = projname
        self.session  = session
        self.user     = user
        self.seq      = seq
        self.ts       = ts
        self.sql      = sql
        self.uuid     = "{} [{} {}] {}.{}.{}".format(type(self).__name__, seq, ts, projname, session, user)
