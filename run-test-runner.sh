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


if [ $# -ne 8 ]; then
    echo "usage: ./run-test.sh <vm-name> <vm-ip> <start-state-name> <selenese-login-file> <command-wait> <firefox> <csrf-db> <test-id>"
    exit 1
fi


python=python

csrf_start_relative="./csrf-test-runner/"
mosgi_start_relative="./mosgi/src/"

vm_name=$1
vm_ip=$2
vm_root=root
vm_pwd=bitnami
start_state_name=$3
base_url="http://${vm_ip}"
login_tc=$4
wait=$5
firefox=$6
csrf_test_db=$7
test_id=$8


db_postfix=".db"
log_postfix=".log"


inter_com_port=8844


vilanoo_folder="${HOME}/.vilanoo/"
timestamp=`date '+%Y%m%d%H%M'`
csrf_log_path="${vilanoo_folder}csrf-${timestamp}-test-runner-${test_id}${db_postfix}"
mosgi_db_path="${vilanoo_folder}csrf-${timestamp}-mosgi-${test_id}${db_postfix}"
mosgi_log_path="${vilanoo_folder}csrf-${timestamp}-mosgi-${test_id}${log_postfix}"
mosgi_php_session_folder="/opt/bitnami/php/tmp/"
mosgi_xdebug_trace_file="/tmp/xdebug.xt"
mosgi_listen_port=8888
mosgi_listen_ip="127.0.0.1"
mosgi_root_user="root"
mosgi_root_pwd="bitnami"




if vboxmanage list vms | grep --quiet "\"${vm_name}\""; then

    if vboxmanage list runningvms | grep --quiet "\"${vm_name}\""; then
        echo "test vm ${vm_name} is currently running - shut down before trying again"
        exit 1
    else
        echo "restoring snapshot"
        echo `vboxmanage snapshot ${vm_name} restore ${start_state_name}`
        echo "starting up machine"
        echo `vboxmanage startvm ${vm_name} --type headless`
        echo "everything done"
    fi
else
    echo "machine ${vm_name} is unknown"
    exit 1
fi

# setup mosgi db
db_dump_schema="./data/DBSchemaDump.sql"
# cat ${db_dump_schema} | xargs echo
cat ${db_dump_schema} | sqlite3 ${mosgi_db_path}


# finishing for vm startup
echo "waiting for guest to finish starting up..."
sleep 4

TMUX_SESSION="csrf-test-${test_id}"

echo tmux new -s ${TMUX_SESSION} "cd ${mosgi_start_relative}; ./run.sh -P ${mosgi_php_session_folder} -x ${mosgi_xdebug_trace_file} -p ${mosgi_listen_port} -i ${mosgi_listen_ip} -t ${vm_ip} -r ${vm_root} -c ${vm_pwd} -s ${mosgi_db_path} > >(tee ${mosgi_log_path}) 2> >(tee ${mosgi_log_path}); sleep 10"\; \
                                  split-window -h "cd ${csrf_start_relative}; sleep 8; ./test-runner.py -b ${base_url} -t ${test_id} -M ${mosgi_listen_ip} -P ${mosgi_listen_port} -d ${csrf_test_db} -S ${login_tc} -w ${wait} --replace-cookie > >(tee ${csrf_log_path}) 2> >(tee ${csrf_log_path}); sleep 30" \; attach \;


tmux new -s ${TMUX_SESSION} "pwd; cd ${mosgi_start_relative}; ./run.sh -P ${mosgi_php_session_folder} -x ${mosgi_xdebug_trace_file} -p ${mosgi_listen_port} -i ${mosgi_listen_ip} -t ${vm_ip} -r ${vm_root} -c ${vm_pwd} -s ${mosgi_db_path} > >(tee ${mosgi_log_path}) 2> >(tee ${mosgi_log_path}); sleep 30" \; \
                             split-window -h "cd ${csrf_start_relative}; sleep 8; ./test-runner.py -b ${base_url} -t ${test_id} -M ${mosgi_listen_ip} -P ${mosgi_listen_port} -d ${csrf_test_db} -S ${login_tc} -w ${wait} --selenese-args \"--firefox ${firefox} --height 2048 --width 2048\" --replace-cookie > >(tee ${csrf_log_path}) 2> >(tee ${csrf_log_path}); sleep 30" \; attach \;


echo `vboxmanage controlvm ${vm_name} poweroff`
