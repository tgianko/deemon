from core import *
from selenese import *
from sql import *


class HTTPRequest(Observation, CausalNode):
    
    method = Property()
    url = Property()

    URL         = RelatedTo("URL")
    Header      = RelatedTo("HeaderList")
    ABSTRACTSTO = RelatedTo("AbstractHTTPRequest")
# We can use any other type of node.
# Apparently this library does not to type enforcement for nodes.
    Body        = RelatedTo("Body")
    Next        = RelatedTo("HTTPRequest")
    Transaction = RelatedTo("HTTPResponse")
    Causes      = RelatedTo("SQLQuery")

    def __init__(self, projname, session, user, seq, ts, method, url):
        super(HTTPRequest, self).__init__(projname, session, user, seq, ts)
        self.method = method
        self.url = url
        self.uuid = "{} [{} {}] {}.{}.{}".format(type(self).__name__, seq, ts, projname, session, user)


class AbstractHTTPRequest (AbstractObservation):
    
    Abstracts   = RelatedTo(HTTPRequest)
    ABSTRACTSTO = RelatedFrom("HTTPRequest", "ABSTRACTSTO")

    def __init__(self, projname, session, user, hashvalue):
        super(AbstractHTTPRequest, self).__init__(projname, session, user)
        self.uuid = hashvalue


class HTTPResponse(Observation):
    
    status = Property()

    Header = RelatedTo("HeaderList")
# We can use any other type of node.
# Apparently this library does not to type enforcement for nodes.
    Body = RelatedTo("Body")
    
    def __init__(self, projname, session, user, seq, ts, status):
        super(HTTPResponse, self).__init__(projname, session, user, seq, ts)
        self.status = status
        self.uuid = "{} [{} {}] {}.{}.{}".format(type(self).__name__,
                                                 seq, ts, projname,
                                                 session, user)


class URL(BasicNode):

    __primarykey__ = "uuid"

    uuid = Property()

    projname = Property()
    
    url = Property()

    Scheme      = RelatedTo(DataValue)
    Netloc      = RelatedTo(DataValue)
    Params      = RelatedTo(DataValue)
    Fragment    = RelatedTo(DataValue)
    Path        = RelatedTo(DataValue)
    QueryString = RelatedTo(KeyValuePair)

    def __init__(self, projname, url):
        super(URL, self).__init__(projname)
        self.url = url

class HeaderList(BasicNode):

    Header = RelatedTo(KeyValuePair)

    def __init__(self, projname):
        super(HeaderList, self).__init__(projname)


class Body(BasicNode):
    
    content_type = Property()

    Contains = RelatedTo([DataValue, KeyValuePair])

    def __init__(self, projname, ctype):
        super(Body, self).__init__(projname)
        self.content_type = ctype
