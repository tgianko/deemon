from core import *
from http import *
from sql import *


class SeleneseCommand(Observation, CausalNode):
   
    command  = Property()
    target   = Property()
    value    = Property()

    Command  = RelatedTo(DataValue)
    Target   = RelatedTo(DataValue)
    Value    = RelatedTo(DataValue)
    Next     = RelatedTo("SeleneseCommand")

    Causes   = RelatedTo(HTTPRequest)

    def __init__(self, projname, session, user, seq, ts, command, target, value):
        super(SeleneseCommand, self).__init__(projname, session, user, seq, ts)
        self.command = command
        self.target  = target
        self.value   = value
        self.uuid     = "{} [{} {}] {}.{}.{}".format(type(self).__name__, seq, ts, projname, session, user)