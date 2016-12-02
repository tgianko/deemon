#!/bin/bash

amount_queries=`echo "SELECT count(*) FROM sql_queries;" | sqlite3 ${analyzed_db_path}`
amount_queries_last=`echo "SELECT count(*) FROM sql_queries WHERE http_request_id = (SELECT max(id) FROM http_requests);" | sqlite3 ${analyzed_db_path}`
amount_prop_queries=`grep "PDO->prepare" /tmp/analysis-query-result-buffer | grep -o "INSERT\|UPDATE\|DELETE\|SET" | wc -l`

if [ "${amount_queries}" -lt "5" ]; then
    echo -e "\033[0;33moverall amount queries: ${amount_queries}\033[0m"
elif [ "${amount_queries}" = "0" ]; then
    echo -e "\033[0;31moverall amount queries: ${amount_queries}\033[0m"
else
    echo "overall amount queries: ${amount_queries}"
fi

echo "amount queries in db for last request: ${amount_queries_last}"
if [ "${amount_queries_last}" = "${amount_prop_queries}" ]; then
    echo -e "amount queries for INSERT|UPDATE|DELETE|SET in tmp file for last request xdebug: ${amount_prop_queries}"
else
    echo -e "\033[0;31mamount queries for INSERT|UPDATE|DELETE|SET in tmp file for last request xdebug: ${amount_prop_queries}\033[0m"
fi
