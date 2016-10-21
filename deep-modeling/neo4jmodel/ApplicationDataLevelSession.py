from enum import Enum
from GenericElements import DataValue
from py2neo.ogm import GraphObject, Property, RelatedTo
import datetime


class SessionElementType(Enum):
    string = 1
    integer = 2
    array = 3
    empty = 4
    boolean = 5


class SessionElement():
    type = None
    content = None


class SessionElementEmpty(GraphObject, SessionElement):
    type = SessionElementType.empty

    __primarykey__ = "uuid"

    projname = Property()
    uuid = Property()

    def __init__(self, projname=None):
        self.uuid = "{}".format(datetime.datetime.now())
        self.projname = projname

    def getValue(self):
        return None

    def inject(self, graph):
        graph.push(self)


class SessionElementString(GraphObject, SessionElement):
    type = SessionElementType.string

    __primarykey__ = "uuid"

    uuid = Property()
    projname = Property()

    value = RelatedTo("DataValue")

    def __init__(self, content, projname=None):
        self.uuid = "{}".format(datetime.datetime.now())
        self.projname = projname
        self.value.add(DataValue(value=content))

    def getValue(self):
        return list(self.value)[0].value

    def inject(self, graph):
        graph.push(self)


class SessionElementInteger(GraphObject, SessionElement):
    type = SessionElementType.integer

    __primarykey__ = "uuid"

    projname = Property()
    uuid = Property()

    value = RelatedTo("DataValue")

    def __init__(self, content, projname=None):
        self.uuid = "{}".format(datetime.datetime.now())
        self.projname = projname
        self.value.add(DataValue(value=content))

    def getValue(self):
        return list(self.value)[0].value
 
    def inject(self, graph):
        pass


class SessionElementBoolean(GraphObject, SessionElement):
    type = SessionElementType.boolean

    __primarykey__ = "uuid"

    projname = Property()
    uuid = Property()

    value = RelatedTo("DataValue")

    def __init__(self, content, projname=None):
        self.uuid = "{}".format(datetime.datetime.now())
        self.projname = projname
        self.value.add(DataValue(value=content))

    def getValue(self):
        return list(self.value)[0].value
 
    def inject(self, graph):
        pass


class SessionElementArrayElement(GraphObject):
    
    __primarykey__ = "uuid"

    projname = Property()
    uuid = Property()

    key = RelatedTo("SessionElementString")
    value = RelatedTo("SessionElementString")

    def __init__(self, key, value, projname=None):
        self.uuid = "{}".format(datetime.datetime.now())
        self.projname = projname
        self.key.add(key)
        self.value.add(value)

    def inject(self, graph):
        graph.push(self)


class SessionElementArray(GraphObject, SessionElement):
    type = SessionElementType.array

    __primarykey__ = "uuid"

    projname = Property()
    uuid = Property()

    contains = RelatedTo("SessionElementArrayElement")

    def __init__(self, content, projname=None):
        self.ident = "{}".format(datetime.datetime.now())
        self.projname = projname
        for key, value in content.iteritems():
            self.contains.add(SessionElementArrayElement(
                SessionElementString(key),
                value))

    def inject(self, graph):
        graph.push(self)


class SessionContent(GraphObject):

    __primarykey__ = "uuid"
    
    projname = Property()
    uuid = Property()
    ident = Property()

    name = RelatedTo("DataValue")
    contains = RelatedTo("SessionElementArray")

    def __init__(self, name, content, projname=None):
        self.ident = name
        self.uuid = "{}".format(datetime.datetime.now())
        self.projname = projname
        self.name.add(DataValue(value=name))
        self.contains.add(content)

    def inject(self, graph):
        graph.push(self)


class PHPSession(GraphObject):

    __primarykey__ = "uuid"

    projname = Property()
    uuid = Property()
    ident = Property()

    name = RelatedTo("DataValue")
    contains = RelatedTo("SessionContent")

    def __init__(self, name, content, projname=None):
        self.ident = name
        self.uuid = "{}".format(datetime.datetime.now())
        self.projname = projname
        self.name.add(DataValue(value=name))
        for cont in content:
            self.contains.add(cont)

    def inject(self, graph):
        graph.push(self)

