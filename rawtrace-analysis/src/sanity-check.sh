#!/bin/bash

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
