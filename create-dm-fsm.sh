#!/bin/bash

# I made this script globally applicable by removing
# all the references which seem to be not directly
# needed for the script to run and cannot be resolved
# by my system setup

#. job_pool.sh

#source create-shared.sh

#CSRF_PATH="${DB_PATH}/csrf-test-runner/"
if [ $# -ne 2 ]; then
    echo "usage: ./create-dm-fsm.sh <csv-file-containing-projname-session-user-mappings> <vilanoo-virtenv>
    exit 1
fi


PROJ_PATH=`pwd`
SOURCE_PATH=${2}
MAPPING_SOURCE_CSV=${1}

function whatever {
    cd ${PROJ_PATH}/deep-modeling/
    source ${SOURCE_PATH}
    echo ./dbmanager.py analysis inference ${1} ${2} ${3} ${4}
    ./dbmanager.py analysis inference ${1} ${2} ${3} &> ${4}
    deactivate
}

#job_pool_init 12 0

while IFS=',' read PROJNAME SESSION USER
do
    LOG_FILE="${HOME}/.vilanoo/${PROJNAME}-${SESSION}-${USER}-dbmanager-fsm.log"

    whatever ${PROJNAME} ${SESSION} ${USER} ${LOG_FILE}
\
done < $MAPPING_SOURCE_CSV #mapping_projname_session_user.csv

#job_pool_shutdown
#echo "job_pool_nerrors: ${job_pool_nerrors}"
