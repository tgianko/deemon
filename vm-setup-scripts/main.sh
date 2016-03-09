#!/bin/bash


$vm_vdi=$1
$vm_name="bikini-atoll"
$database_type=$2
$host_ip="NOT SET"
$incoming_port='4444'
$outgoing_port='5555'
$com_port='7777'

hostonlyifs_count=`vboxmamage list hostonlyifs | grep GUID | wc -l`
if [ $hostonlyifs_count -eq 1 ]; then
    $host_ip=`vboxmanage list hostonlyifs | grep IPAddress | sed 's/IPAddress:[ ]*//'`
else
    echo "ERROR: there are currently $hostonlyifs_count hostonly vbox networks"
    echo "there must only be one. Resolve problem and restart script"
    exit 1
fi


sudo ./set_up_bitnami_harddrive.sh $vm_vid $database_type $host_ip $com_port


vboxmanage startvm $vm_name --type headless

$guest_ip=`nc -l $com_port`

echo "guest system name is : $vm_name"
echo "guest IP             : $guest_ip"
echo "outgoing port        : $outgoing_port"
echo "incoming port        : $incoming_port"

exit 0
