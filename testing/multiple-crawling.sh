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


if [ $# -ne 2 ]; then
    echo "it is assumed that the ip of the target webapp is"
    echo "192.168.56.101 and the proyx is running on"
    echo "127.0.0.1:4242 and the bikini-atoll VM is not running and"
    echo "is virgin in the sense that it will do a full boot up"
    echo ""
    echo "usage: ./multiple-crawling.sh <crawling-name> <amount-crawling-runs>"
    exit 1
fi


create-timestamp() {
    local result=`date +"%Y%m%d%H%M"`
    echo "$result"
}


com_port='7777'
crawler_main_folder="${HOME}/hiwi/csrf/jAEk/crawler/"
vm_id="bikini-atoll"
timestamp_string=`create-timestamp`
folder_string="${HOME}/.vilanoo/crawlingrun-${1}-${timestamp_string}/"

echo "checking & creating folder ${folder_string}"

if [ -d ${folder_string} ]; then
    echo "there is already such a crawling folder - wait a minute Mr. Impatient"
    exit 1
else
    mkdir ${folder_string}
fi

#reset the database 
echo "resetting the database"
rm -f "${HOME}/.vilanoo/vilanoo.db"

#establish a snapshot
echo "establishing pre-crawling snasphot"
snapshot_name="${1}-prespider-snapshot"
vboxmanage startvm ${vm_id}
nc -l ${com_port}
vboxmanage snapshot ${vm_id} take "${snapshot_name}"

for (( c=0; c<=${2}; c++ )) 
do
    echo ""
    echo "----- SPIDERRUN ${c} START -----"
    echo ""

    #start a crawling
    pushd `pwd`
    cd ${crawler_main_folder}
    python3 main.py
    popd

    #move db to folder
    currtime=`create-timestamp`
    mv "${HOME}/.vilanoo/vilanoo.db" "${folder_string}/spiderun-${currtime}.db"
    echo "saved spiderrun db"


    #revert to snapshot
    echo "revert vm to pre-spider state"
    vboxmanage controlvm ${vm_id} poweroff
    vboxmanage snapshot ${vm_id} restore "${snapshot_name}"
    vboxmanage startvm ${vm_id}
    #nc -l ${com_port} not needed as the snapshot is already started
    echo "done"

    echo ""
    echo "----- SPIDERRUN ${c} DONE -----"
    echo ""
    sleep 1

done
    

