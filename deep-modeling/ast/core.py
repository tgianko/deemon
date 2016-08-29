from py2neo.ogm import GraphObject, Property, RelatedTo
from uuid import uuid4

class DataValue(GraphObject):

    __primarykey__ = "uuid"

    uuid  = Property()

    projname = Property()

    value = Property()
    tags  = Property()
    
    def __init__(self, projname=None, value=None, tags=[]):
        self.projname=projname
        self.value = value
        self.tags=tags
        self.uuid = str(uuid4()) # I don't understand why without this RelatedTo does not wor

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


"""
This is a key=value data
"""
class KeyValuePair(GraphObject):

    __primarykey__ = "uuid"

    uuid  = Property()

    projname = Property()
    
    key   = Property()
    value = Property()
    tags  = Property()

    uuid  = Property()

    def __init__(self, projname=None, key=None, value=None, tags=[]):
        self.projname=projname
        self.key   = key
        self.value = value
        self.tags  = tags
        self.uuid  = str(uuid4())

class URL(GraphObject):

    __primarykey__ = "uuid"

    uuid  = Property()

    projname = Property()
    
    url = Property()

    Scheme      = RelatedTo("DataValue")
    Netloc      = RelatedTo("DataValue")
    Params      = RelatedTo("DataValue")
    Fragment    = RelatedTo("DataValue")
    Path        = RelatedTo("DataValue")
    QueryString = RelatedTo("KeyValuePair")

    def __init__(self, projname=None, url=None):
        self.projname=projname
        self.url = url
        self.uuid  = str(uuid4())


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

class HeaderList(GraphObject):

    __primarykey__ = "uuid"

    uuid  = Property()

    projname = Property()
    
    Header = RelatedTo("KeyValuePair")

    def __init__(self, projname=None):
        self.projname=projname
        self.uuid  = str(uuid4())


class Body(GraphObject):

    __primarykey__ = "uuid"

    uuid  = Property()

    projname = Property()
    
    content_type = Property()

    Contains = RelatedTo("DataValue")

    def __init__(self, projname=None, ctype=None):
        super(Body, self).__init__()
        self.content_type = ctype
        self.uuid  = str(uuid4())



"""
Observation: Observations have a sequence number and a timestamp.

An observation is unique.
"""

class SeleneseCommand(GraphObject):

    __primarykey__ = "uuid"

    uuid  = Property()
    
    projname = Property()
    session  = Property()
    user     = Property()
    seq      = Property()
    ts       = Property()

    command  = Property()
    target   = Property()
    value    = Property()

    Command  = RelatedTo("DataValue")
    Target   = RelatedTo("DataValue")
    Value    = RelatedTo("DataValue")
    Next     = RelatedTo("SeleneseCommand")

    Caused   = RelatedTo("HTTPRequest")

    def __init__(self, projname=None, session=None, user=None, seq=None, ts=None, command=None, target=None, value=None):
        self.projname=projname
        self.session = session
        self.user    = user
        self.seq     = seq
        self.ts      = ts
        self.command = command
        self.target  = target
        self.value   = value
        self.uuid     = "{} [{} {}] {}.{}.{}".format(type(self).__name__, seq, ts, projname, session, user)


class HTTPRequest(GraphObject):
    
    __primarykey__ = "uuid"

    uuid  = Property()

    projname = Property()
    
    session  = Property()
    user     = Property()
    seq      = Property()
    ts       = Property()
    
    method   = Property()
    url      = Property()
    
    URL      = RelatedTo("URL")
    Header   = RelatedTo("HeaderList")
    Body     = RelatedTo("Body") # We can use any other type of node. Apparently this library does not to type enforcement for nodes.

    Next     = RelatedTo("SeleneseCommand")
    Transaction = RelatedTo("HTTPResponse")
    Caused   = RelatedTo("SQLQuery")

    def __init__(self, projname=None, session=None, user=None, seq=None, ts=None, method=None, url=None):
        self.projname = projname
        self.session  = session
        self.user     = user
        self.seq      = seq
        self.ts       = ts
        self.method   = method
        self.url      = url
        self.uuid     = "{} [{} {}] {}.{}.{}".format(type(self).__name__, seq, ts, projname, session, user)


class HTTPResponse(GraphObject):
    
    __primarykey__ = "uuid"

    uuid  = Property()

    projname = Property()
    
    session  = Property()
    user     = Property()
    seq      = Property()
    ts       = Property()
    
    status      = Property()

    Header      = RelatedTo("HeaderList")
    Body        = RelatedTo("Body") # We can use any other type of node. Apparently this library does not to type enforcement for nodes.
    

    def __init__(self, projname=None, session=None, user=None, seq=None, ts=None, status=None):
        self.projname = projname
        self.session  = session
        self.user     = user
        self.seq      = seq
        self.ts       = ts
        self.status = status
        self.uuid     = "{} [{} {}] {}.{}.{}".format(type(self).__name__, seq, ts, projname, session, user)


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
