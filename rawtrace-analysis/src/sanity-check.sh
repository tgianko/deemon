#!/bin/bash
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


analyzed_db_path=$1

amount_queries=`echo "SELECT count(*) FROM sql_queries;" | sqlite3 ${analyzed_db_path}`
amount_queries_last=`echo "SELECT count(*) FROM sql_queries WHERE http_request_id = (SELECT max(id) FROM http_requests);" | sqlite3 ${analyzed_db_path}`

if [ "${amount_queries}" -lt "5" ]; then
    echo -e "\033[0;33moverall amount queries: ${amount_queries}\033[0m"
elif [ "${amount_queries}" = "0" ]; then
    echo -e "\033[0;31moverall amount queries: ${amount_queries}\033[0m"
else
    echo -e "\033[0;34moverall amount queries: ${amount_queries}\033[0m"
fi

if [ "${amount_queries_last}" = "0" ]; then
    echo -e "\033[0;33mamount queries in db for last request: ${amount_queries_last}\033[0m"
else
    echo -e "\033[0;34mamount queries in db for last request: ${amount_queries_last}\033[0m"
fi
