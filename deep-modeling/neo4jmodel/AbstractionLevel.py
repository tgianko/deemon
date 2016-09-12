from py2neo.ogm         import GraphObject, Property, RelatedTo


class AbstractQuery(GraphObject):
    
    __primarykey__ = "hash"

    hash = Property()
    projname = Property()

    def __init__(self,hash):
        self.hash = hash
        self.projname = "" #TODO:initialize this


class AbstractHTTPRequest (GraphObject):

    __primarykey__ = "id"

    id = Property()
    projname = Property()

    def __init__(self):
        self.id = ""
        self.projname = ""
    
