#!/bin/bash

if [ $# -ne 5 ]; then
    echo "./vilanoo.sh <bind_address> <bind_port> <mysql_proxy_port> <mysql_server_host> <mysql_server_port>"
    exit
fi


pwd_path=`pwd`
netlib="$pwd_path/../lib/netlib/"
libmproxy="$pwd_path/../lib/mitmproxy/"

PYTHONPATH="$netlib:$libmproxy" python main.py -b $1 -p $2 --mysql-proxy-bin /opt/mysql-proxy-0.8.5-linux-glibc2.3-x86-64bit/bin/mysql-proxy  --mysql-proxy-port $3 --mysql-server-host $4 --mysql-server-port $5 --state-changing-requests -s inlinescript.py
