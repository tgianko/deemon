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
