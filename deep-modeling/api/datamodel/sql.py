from core import *
from selenese import *
from http import *


class SQLToken(DataValue):

    ttype = Property()

    def __init__(self, projname, ttype, value, tags=[]):
        super(SQLToken, self).__init__(projname, value, tags)
        self.ttype = ttype


class SQLTokenList(BasicNode):

    Child = RelatedTo("SQLToken")

    def __init__(self, projname):
        super(SQLTokenList, self).__init__(projname)


class SQLStatement(BasicNode):

    ttype = Property()
    stmt = Property()

    Child = RelatedTo("SQLToken")

    def __init__(self, projname, statement):
        super(SQLStatement, self).__init__(projname)
        self.stmt = statement


class SQLQuery(Observation):
    
    sql = Property()

    Statement = RelatedTo("SQLStatement")
    ABSTRACTSTO = RelatedTo("AbstractSQLQuery") # remove this

    def __init__(self, projname, session, user,
                 seq, ts, sql):
        super(SQLQuery, self).__init__(projname, session, user, seq, ts)
        self.sql = sql
        self.uuid = "{} [{} {}] {}.{}.{}".format(type(self).__name__,
                                                 seq, ts, projname,
                                                 session, user)

class AbstractSQLQuery(AbstractObservation):
    
    Abstracts = RelatedTo("SQLQuery")
    ABSTRACTSTO = RelatedFrom("SQLQuery", "ABSTRACTSTO") # remove this
    def __init__(self, projname, session, user, hashval):
        super(AbstractSQLQuery, self).__init__(projname, session, user)
        self.uuid = hashval

