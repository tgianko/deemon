#!/bin/bash


if [ $# -ne 2 ]; then
    echo "usage: ./script.sh </full/path/to/vm.vdi> <database_type_of_webapp>"
    exit 1
fi


vm_vdi=$1
vm_name="bikini-atoll"
database_type=$2
host_ip="NOT SET"
incoming_port='4444'
outgoing_port='5555'
com_port='7777'

hostonlyifs_count=`vboxmanage list hostonlyifs | grep GUID | wc -l`
if [ "$hostonlyifs_count" == "1" ]; then
    host_ip=`vboxmanage list hostonlyifs | grep IPAddress | sed 's/IPAddress:[ ]*//'`
else
    echo "ERROR: there are currently $hostonlyifs_count hostonly vbox networks"
    echo "there must only be one. Resolve problem and restart script"
    exit 1
fi


echo "sudo ./bitnami_setup_vm_harddrive.sh $vm_vdi $database_type $host_ip $com_port"
sudo ./bitnami_setup_vm_harddrive.sh $vm_vdi $database_type $host_ip $com_port

if [ $? -ne 0 ]; then
    exit $?
fi


./vbox_configure_vm.sh $vm_vdi $vm_name

if [ $? -ne 0 ]; then
    exit $?
fi


vboxmanage startvm $vm_name #--type headless

echo "waiting for guest to finish starting up..."
guest_ip=`nc -l $com_port`

echo ""
echo "guest system name is : $vm_name"
echo "guest IP             : $guest_ip"
echo "outgoing port        : $outgoing_port"
echo "incoming port        : $incoming_port"
echo "run redir --lport 8080 --caddr $guest_ip --cport=80"
echo "to reach the webapp via 127.0.0.1:8080"

exit 0
