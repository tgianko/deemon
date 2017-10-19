# This file is part of Deemon.

# Deemon is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# Deemon is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with Deemon.  If not, see <http://www.gnu.org/licenses/>.

from dm_types import *


def analysis_oppat_bulk(arg, graph, logger):
    query = """MATCH init=(apt:AbstractParseTree)-[:ABSTRACTS]->(pt:ParseTree),
                    trace=(pt)-[:PARSES]->(e:Event)
     RETURN DISTINCT apt.uuid AS apt_uuid, e.projname AS projname, e.session AS session, e.user AS user"""

    rs = graph.run(query)
    rs = list(rs)

    for i, r in enumerate(rs):
        abspt_uuid = r["apt_uuid"]
        projname = r["projname"]
        session = r["session"]
        user = r["user"]

        label = infer_trace_patterns(graph, abspt_uuid, projname, session, user, logger)
        logger.debug("Adding operation pattern {} for {} {} {} {} ({}/{})".format(label, abspt_uuid, projname, session, user, i, len(rs)))


def infer_event_patterns(abspt_uuid):
    """
    EVT_UNIQUE_OP or EVT_REPEATED_OP?
    """
    pass


def infer_trace_patterns(graph, abspt_uuid, projname, session, user, logger):
    """
    TRACE_SINGLETON_OP or TRACE_REPEATED_OP?
    """

    query = """MATCH init=(apt:AbstractParseTree {uuid:{abspt_uuid}})-[:ABSTRACTS]->(pt:ParseTree), 
                    trace=(pt)-[:PARSES]->(e:Event {dm_type:{dm_type}, projname:{projname}, session:{session}, user:{user}}) 
                WITH DISTINCT apt, e
              RETURN apt, count(e) AS count;"""

    data = {
        "abspt_uuid": abspt_uuid,
        "dm_type": XDEBUG,
        "projname": projname,
        "session": session,
        "user": user
    }

    rs = graph.run(query, data)
    rs = list(rs)

    assert len(rs) == 1

    count = rs[0]["count"]
    
    if count == 1:
        return TRACE_SINGLETON_OP
    
    return TRACE_REPEATED_OP


def is_absevt_op(abspt_uuid, absevt_uuid):
    pass
