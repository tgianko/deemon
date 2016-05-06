#!/bin/bash

if [ $# -ne 6 ]; then
    echo "./vilanoo.sh <bind_proxy_address> <bind_port> <mysql_proxy_ip> <mysql_proxy_port> <mysql_server_host> <mysql_server_port>"
    exit
fi


pwd_path=`pwd`
netlib="$pwd_path/../lib/netlib/"
#libmproxy="$pwd_path/../lib/mitmproxy/"
#mysqlproxy="/opt/mysql-proxy-0.8.5-linux-glibc2.3-x86-64bit/bin/mysql-proxy"
mysqlproxy="/home/simkoc/libs/mysql-proxy-0.8.5-linux-debian6.0-x86-64bit/bin/mysql-proxy"

PYTHONPATH="$netlib:$libmproxy" python main.py -b $1 -p $2 --mysql-proxy-bin ${mysqlproxy}  --mysql-proxy-address ${3} --mysql-proxy-port ${4} --mysql-server-host $5 --mysql-server-port $6 --state-changing-requests -s inlinescript.py
