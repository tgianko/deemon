from py2neo.ogm import GraphObject, Property, RelatedTo, RelatedFrom
from uuid import uuid4
from GenericElements import KeyValuePair,DataValue
from ApplicationDataLevelSQL import SQLQuery


class HTTPRequest(GraphObject):
    
    __primarykey__ = "uuid"

    uuid = Property()

    projname = Property()
    
    session = Property()
    user = Property()
    seq = Property()
    ts = Property()
    
    method = Property()
    url = Property()
    
    URL = RelatedTo("URL")
    Header = RelatedTo("HeaderList")
    ABSTRACTSTO = RelatedTo("AbstractHTTPRequest")
# We can use any other type of node.
# Apparently this library does not to type enforcement for nodes.
    Body = RelatedTo("Body")

    Next = RelatedTo("HTTPRequest")
    Transaction = RelatedTo("HTTPResponse")
    Caused = RelatedTo("SQLQuery")

    def __init__(self, projname=None, session=None, user=None,
                 seq=None, ts=None, method=None, url=None):
        self.projname = projname
        self.session = session
        self.user = user
        self.seq = seq
        self.ts = ts
        self.method = method
        self.url = url
        self.uuid = "{} [{} {}] {}.{}.{}".format(type(self).__name__,
                                                 seq, ts, projname,
                                                 session, user)


class AbstractHTTPRequest (GraphObject):

    __primarykey__ = "hash"

    hash = Property()
    projname = Property()
    
    ABSTRACTSTO = RelatedFrom("HTTPRequest", "ABSTRACTSTO")

    def __init__(self, hash):
        self.hash = hash
        self.projname = ""


class HTTPResponse(GraphObject):
    
    __primarykey__ = "uuid"

    uuid = Property()

    projname = Property()
    
    session = Property()
    user = Property()
    seq = Property()
    ts = Property()
    
    status = Property()

    Header = RelatedTo("HeaderList")
# We can use any other type of node.
# Apparently this library does not to type enforcement for nodes.
    Body = RelatedTo("Body")
    
    def __init__(self, projname=None, session=None, user=None,
                 seq=None, ts=None, status=None):
        self.projname = projname
        self.session = session
        self.user = user
        self.seq = seq
        self.ts = ts
        self.status = status
        self.uuid = "{} [{} {}] {}.{}.{}".format(type(self).__name__,
                                                 seq, ts, projname,
                                                 session, user)


class URL(GraphObject):

    __primarykey__ = "uuid"

    uuid = Property()

    projname = Property()
    
    url = Property()

    Scheme = RelatedTo("DataValue")
    Netloc = RelatedTo("DataValue")
    Params = RelatedTo("DataValue")
    Fragment = RelatedTo("DataValue")
    Path = RelatedTo("DataValue")
    QueryString = RelatedTo("KeyValuePair")

    def __init__(self, projname=None, url=None):
        self.projname = projname
        self.url = url
        self.uuid = str(uuid4())


class HeaderList(GraphObject):

    __primarykey__ = "uuid"

    uuid = Property()

    projname = Property()
    
    Header = RelatedTo("KeyValuePair")

    def __init__(self, projname=None):
        self.projname = projname
        self.uuid = str(uuid4())


class Body(GraphObject):

    __primarykey__ = "uuid"

    uuid = Property()

    projname = Property()
    
    content_type = Property()

    Contains = RelatedTo("DataValue")

    def __init__(self, projname=None, ctype=None):
        super(Body, self).__init__()
        self.content_type = ctype
        self.uuid = str(uuid4())


