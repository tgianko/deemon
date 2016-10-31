"""
Data model of the deep modeling framework
"""
from py2neo.ogm import *
from uuid import uuid4

class BasicNode(GraphObject):
    """ Any node in a deep model DB must extend this class
    """

    __primarykey__ = "uuid"

    uuid     = Property()

    projname = Property()

    dm_type  = Property()

    def __init__(self, projname, dm_type=None):
        self.projname = projname
        self.dm_type  = dm_type
        self.uuid = str(uuid4())


"""
**************************
        DFA
**************************
"""

class DFAState(BasicNode):
    """ Describe a state of a DFA
    """

    state_id   = Property()

    HasTransition = RelatedTo("DFAStateTransition")

    def __init__(self, projname, dm_type, state_id):
        super(DFAState, self).__init__(projname, dm_type, state_id)

class DFAStateTransition(BasicNode):
    """ Describe a transition between states
    """

    accepted = Property()

    To = RelatedTo("FSMState")
    Accepts = RelatedTo(["Event", "ParseTree"])
    
    def __init__(self, projname, dm_type, symbol):
        super(DFAStateTransition, self).__init__(projname, dm_type)


"""
**************************
        TRACE
**************************
"""

class Event(BasicNode):
    """ Describe an observation of a dynamic trace
    """

    session = Property()
    user    = Property()
    seq     = Property()
    ts      = Property()
    message = Property()

    IsFollowedBy = RelatedTo("Event")
    Caused       = RelatedTo("Event")

    def __init__(self, projname, dm_type, session, user, seq, ts, message):
        super(Event, self).__init__(projname ,dm_type)
        self.session = session
        self.user    = user
        self.seq     = seq
        self.ts      = ts
        self.message = message
        self.uuid = "{} - {}.{}.{}.{}.{}".format(dm_type, projname, session, 
                                                 user, seq, ts)


"""
**************************
        PARSE TREE
**************************
"""

class ParseTree(BasicNode):
    """ Root of a parse tree
    """

    dm_type   = Property()
    pos       = Property()
    message   = Property()

    HasChild  = RelatedTo(["PTTerminalNode", "PTNonTerminalNode"])
    Parses    = RelatedTo("Event")

    def __init__(self, projname, dm_type, message, pos=-1):
        super(ParseTree, self).__init__(projname, dm_type)
        self.pos     = pos
        self.message = message



class PTTerminalNode(BasicNode):
    """ Terminal node of a parse tree
    """

    dm_type   = Property()
    symbol    = Property()
    s_type    = Property()
    pos       = Property()

    def __init__(self, projname, dm_type, symbol, s_type, pos):
        super(PTTerminalNode, self).__init__(projname, dm_type)
        self.symbol = symbol
        self.s_type = s_type
        self.pos = pos


class PTNonTerminalNode(BasicNode):
    """ Non terminal node of a parse tree
    """

    dm_type   = Property()
    s_type    = Property()
    pos       = Property()
    HasChild  = RelatedTo(["PTTerminalNode", "PTNonTerminalNode", "ParseTree"]) # Here is a ParseTree for Hierarchical parse trees

    def __init__(self, projname, dm_type, s_type, pos):
        super(PTNonTerminalNode, self).__init__(projname, dm_type)

        self.s_type = s_type
        self.pos = pos


"""
**************************
        DATA FLOW
**************************
"""

class Variable(BasicNode):
    """ This represent a basic value
    """

    session      = Property()
    user         = Property()
    seq          = Property()
    name         = Property()
    value        = Property()
    vtype        = Property()

    PropagatesTo = RelatedTo(["Variable"]) # here we list all nodes that can be connected in chains
    HasName      = RelatedTo(["PTTerminalNode"])
    HasValue     = RelatedTo(["PTTerminalNode"])
    BelongsTo    = RelatedTo(["DFAState", "Event"])

    def __init__(self, projname, dm_type, session, user, seq, name, value):
        super(Variable, self).__init__(projname, dm_type)
        self.session = session
        self.user    = user
        self.seq     = seq
        self.value   = value
        self.name    = name
        self.uuid    = "{} - {}.{}.{}.{}.{}.{}".format(dm_type, projname, session, 
                                                 user, seq, name, value.encode("utf-8"))