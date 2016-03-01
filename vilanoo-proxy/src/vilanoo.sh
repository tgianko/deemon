#!/bin/bash
./main.py -b 192.168.56.1 -p 8080 --mysql-proxy-bin /opt/mysql-proxy-0.8.5-linux-glibc2.3-x86-64bit/bin/mysql-proxy  --mysql-proxy-port 3306 --mysql-server-host 192.168.56.4 --mysql-server-port 3306 --state-changing-requests -s inlinescript.py -q
