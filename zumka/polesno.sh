#!/bin/bash

set -e

if [ $# -ne 2 ]; then
    echo "usage: ./polesno.sh </full/path/to/vm.vdi> <vm-name>"
    exit 1
fi


vm_vdi=$1
vm_name=$2
host_ip="NOT SET"
incoming_port='4444'
outgoing_port='5555'
com_port='7777'

#inquire status of vm name 
#exists -> delete
#still runnin -> error



if vboxmanage list vms | grep --quiet "\"$vm_name\""; then
    if vboxmanage list runningvms | grep --quiet "\"$vm_name\""; then
	echo "test vm $vm_name is currently running - shut down before trying again with using die .vdi and polesno.sh"
	exit 1
    else
	vboxmanage unregistervm $vm_name
    fi
fi



hostonlyifs_count=`vboxmanage list hostonlyifs | grep GUID | wc -l`
if [ "$hostonlyifs_count" == "1" ]; then
    host_ip=`vboxmanage list hostonlyifs | grep IPAddress | sed 's/IPAddress:[ ]*//'`
else
    echo "ERROR: there are currently $hostonlyifs_count hostonly vbox networks"
    echo "there must only be one. Resolve problem and restart script"
    exit 1
fi


echo "sudo ./bitnami_setup_vm_harddrive.sh $vm_vdi $host_ip $com_port"
sudo ./bitnami_setup_vm_harddrive.sh $vm_vdi $database_type $host_ip $com_port

if [ $? -ne 0 ]; then
    exit $?
fi


./vbox_configure_vm.sh $vm_vdi $vm_name

if [ $? -ne 0 ]; then
    exit $?
fi


vboxmanage startvm $vm_name --type headless

echo "waiting for guest to finish starting up..."
guest_ip=`nc -l $com_port`
echo "taking snapshot of virgin state..."
vboxmanage snapshot $vm_name take virgin-state
echo "shutting down machine again"
vboxmanage controlvm $vm_name poweroff

echo ""
echo "guest system name is : $vm_name"
echo "guest IP             : $guest_ip"
echo "outgoing port        : $outgoing_port"
echo "incoming port        : $incoming_port"
echo "virgin snapshot      : virgin-state"

exit 0
