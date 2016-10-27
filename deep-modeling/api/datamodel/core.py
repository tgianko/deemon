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

    def __init__(self, projname):
        self.projname = projname
        self.uuid = str(uuid4())

"""
Observation
"""

class Observation(BasicNode):
    """ Describe an observation of a dynamic trace
    """

    session  = Property()
    user     = Property()
    seq      = Property()
    ts       = Property()

    Next     = RelatedTo("Observation")

    def __init__(self, projname, session, user, seq, ts):
        super(Observation, self).__init__(projname)
        self.session = session
        self.user    = user
        self.seq     = seq
        self.ts      = ts

class AbstractObservation(BasicNode):
    """ Describe an abstract observation
    """

    Next     = RelatedTo("AbstractObservation")
    Abstracts = RelatedTo(Observation)

    def __init__(self, projname, session, user):
        super(AbstractObservation, self).__init__(projname)
        self.session = session
        self.user    = user



"""
Finite-State Machines
"""

class FSMStateTransition(BasicNode):
    """ Describe a transition between states
    """

    To = RelatedTo("FSMState")
    
    def __init__(self, projname):
        super(FSMStateTransition, self).__init__(projname)


class FSMState(BasicNode):
    """ Describe a state of a finite-state machine
    """

    state_id   = Property()

    Has = RelatedTo("FSMStateTransition")

    Transition = RelatedTo("FSMState")

    def __init__(self, state_id):
        super(FSMState, self).__init__(projname)
        self.state_id = state_id

"""
Deterministic Finite Automaton
"""

class DFAState(FSMState):
    """ Describe a state of a DFA
    """

    state_id   = Property()

    Has = RelatedTo("DFAStateTransition")

    def __init__(self, state_id):
        super(DFAState, self).__init__(projname, state_id)

class DFAStateTransition(FSMStateTransition):
    """ Describe a transition between states
    """

    accepted = Property()

    To = RelatedTo("FSMState")
    Accepts = RelatedTo(["Observation", "AbstractObservation"])
    
    def __init__(self, projname, symbol):
        super(DFAStateTransition, self).__init__(projname)

"""
Causality
"""

class AbstractFSMState(BasicNode):
    """ Describe a state of a finite-state machine
    """

    Abstracts = RelatedTo(FSMState)
    Transition = RelatedTo("AbstractFSMState")

    def __init__(self, state_id):
        super(AbstractFSMState, self).__init__(projname, state_id)


class CausalNode(GraphObject):
    """ Establishe causality relationship of the type "A Causes B" 
    where A is a class extending CausalNode and B a BasicNode.
    """

    Causes = RelatedTo(BasicNode)


class BasicValue(BasicNode):
    """ This represent a basic value
    """

    value = Property()

    Propagates = RelatedTo(["BasicValue", "DataValue", "KeyValuePair"]) # here we list all nodes that can be connected in chains

    def __init__(self, projname, value):
        super(BasicValue, self).__init__(projname)
        self.value = value



class DataValue(BasicValue):

    tags  = Property()

    def __init__(self, projname, value, tags=[]):
        super(DataValue, self).__init__(projname, value)
        self.tags=tags


class KeyValuePair(BasicValue):

    key   = Property()
    tags  = Property()

    def __init__(self, projname, key, value, tags=[]):
        super(KeyValuePair, self).__init__(projname, value)
        self.key   = key
        self.tags  = tags