from py2neo.ogm         import GraphObject, Property, RelatedTo
from uuid               import uuid4
from GenericElements    import DataValue,KeyValuePair
from BrowserActionLevel import HTTPRequest


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
