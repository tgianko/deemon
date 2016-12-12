#!/bin/bash

# CONF
VILANOO_FOLDER="${HOME}/.vilanoo/csrf-test-runner/"
BIN_PY="/usr/bin/python"
MOSGI_RUN="./mosgi/src/run-mosgi.lisp"
CSRF_RUNNER_FOLDER="./csrf-test-runner/"
TIMESTAMP=`date '+%Y%m%d%H%M'`
DB_POSTFIX=".db"
LOG_POSTFIX=".log"

#default values for bitnami but else these need to become variables
INTER_COM_PORT=8844

MOSGI_PHP_SESSION_FOLDER="/opt/bitnami/php/tmp/"
MOSGI_XDEBUG_TRACE_FILE="/tmp/xdebug.xt"
MOSGI_LISTEN_INTERFACE="127.0.0.1"
MOSGI_ROOT_USER="root"
MOSGI_ROOT_PWD="bitnami"


if [ $# -ne 6 ]; then
    echo "usage: ./run-csrf-test.sh <vm-name> <vm-ip> <test-name> <start-state-name> <csrf-test-file> <mosgi-port> "
    exit 1
fi

# Parameters
VM_NAME=$1
GUEST_IP=$2
TEST_NAME=$3
START_STATE_NAME=$4
CSRF_TEST_FILE=$5
MOSGI_PORT=$6

BASE_URL="http://${GUEST_IP}"

MOSGI_DB_PATH="${VILANOO_FOLDER}${TEST_NAME}-${TIMESTAMP}-csrftests-mosgi${DB_POSTFIX}"
MOSGI_LOG_PATH="${VILANOO_FOLDER}${TEST_NAME}-${TIMESTAMP}-csrftests-mosgi${LOG_POSTFIX}"
CSRFRUNNER_LOG_PATH="${VILANOO_FOLDER}${TEST_NAME}-${TIMESTAMP}-csrftests-csrf_test_runner${LOG_POSTFIX}"
DB_DUMP_SCHEMA="./data/DBSchemaDump.sql"

TOUT=10


function log {
    echo [`date`] -- ${1}
}

function start_vm {
    #check if vm is already running
    #yes -> error
    #no  -> restore virgin snapshot
    if vboxmanage list vms | grep --quiet "\"${VM_NAME}\""; then
        
        if vboxmanage list runningvms | grep --quiet "\"${VM_NAME}\""; then
            log "VM ${VM_NAME} is currently running -- shut down before trying again"
        exit 1
        else
            log `vboxmanage snapshot ${VM_NAME} restore ${START_STATE_NAME}`
            log `vboxmanage startvm ${VM_NAME}`
        fi
        
    else
        log "VM ${VM_NAME} is unknown"
        exit 1
    fi
}

function stop_vm {
    log `vboxmanage controlvm ${VM_NAME} poweroff`
}

function start_mosgi {
    #run MOSGI
    sbcl --dynamic-space-size 10000 --noinform --non-interactive --load ${MOSGI_RUN} --port ${MOSGI_PORT} -P ${MOSGI_PHP_SESSION_FOLDER} -x ${MOSGI_XDEBUG_TRACE_FILE} -i ${MOSGI_LISTEN_INTERFACE} -t ${GUEST_IP} -r ${MOSGI_ROOT_USER}  -c ${MOSGI_ROOT_PWD} -s ${MOSGI_DB_PATH} &>> ${MOSGI_LOG_PATH}
    # add marker in log file
    echo "====================================== MARKER ======================================" >> ${MOSGI_LOG_PATH}
}

function start_csrf_test_runner {
    (cd ${CSRF_RUNNER_FOLDER}; \
    ${BIN_PY} test-runner.py -r ${1} -b ${GUEST_IP} -M ${MOSGI_LISTEN_INTERFACE} -P ${MOSGI_PORT} -d ${CSRF_TEST_FILE} &>>  ${CSRFRUNNER_LOG_PATH})
    # add marker in log file
    echo "====================================== MARKER ======================================" &>>  ${CSRFRUNNER_LOG_PATH}
    

}

#setup MOSGI_DB_PATH
cat ${DB_DUMP_SCHEMA} | sqlite3 ${MOSGI_DB_PATH}

TOT_TESTS=`echo "SELECT count(*) FROM CSRF_tests;" | sqlite3 ${CSRF_TEST_FILE}`
log "Total number of tests to run: ${TOT_TESTS}"
for i in $(seq 1 $TOT_TESTS)
do
    echo $i
    #start vm"
    log "Starting VM..."
    start_vm
    log "Waiting VM to finish up bootstrapping..."
    sleep 4

    log "Running Mosgi..."
    start_mosgi &
    MOSGI_PID=$! 
    log "Mosgi is running (PID=${MOSGI_PID}). Waiting Mosgi to finish up loading..."

    sleep 5

    log "Running csrf-test-runner..."
    start_csrf_test_runner ${i}

    log "Waiting for Mosgi to be done..."
    wait $MOSGI_PID

    log "Halting VM..."
    stop_vm
    #echo "Command:"
    #tmux new -s ${TEST_NAME} "sbcl --dynamic-space-size 10000 --noinform --non-interactive --load ${MOSGI_RUN} --port ${MOSGI_PORT} -P ${MOSGI_PHP_SESSION_FOLDER} -x ${MOSGI_XDEBUG_TRACE_FILE} -i ${MOSGI_LISTEN_INTERFACE} -t ${GUEST_IP} -r ${MOSGI_ROOT_USER}  -c ${mosgi_root_pwd} -s ${MOSGI_DB_PATH} > >(tee ${MOSGI_LOG_PATH}) 2> >(tee ${MOSGI_LOG_PATH}); sleep 10" \; \
    #                                 split-window -h "sleep 10 ; cd ${CSRF_RUNNER_FOLDER}; ${BIN_PY} test-runner.py -b ${GUEST_IP} -M ${MOSGI_LISTEN_INTERFACE} -P ${MOSGI_PORT} -d ${CSRF_TEST_FILE} > >(tee ${CSRFRUNNER_LOG_PATH}) 2> >(tee ${CSRFRUNNER_LOG_PATH}); sleep 30" \; attach \;

done

echo "Done"