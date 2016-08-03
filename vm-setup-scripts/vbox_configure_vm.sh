#!/bin/bash


if [ `vboxmanage list vms | grep $2 | wc -l` -gt 0 ]; then
    echo "ERROR: there exists already vm $2"
    echo "either choose another name or remove this vm"
    exit 1
fi


#clear eventual residue from past vms with that name
rm -rf $HOME/.vilanoo/$2/


#import the vdi as a new virtual machine
vboxmanage createvm --name $2 --basefolder $HOME/.vilanoo/
vboxmanage registervm $HOME/.vilanoo/$2/$2.vbox
vboxmanage storagectl $2 --add sata --name "SCSI Controller"
vboxmanage storageattach $2 --storagectl "SCSI Controller" --medium $1 --port 0 --type hdd


#set up the network adapter
hostonly_adapter_name=`vboxmanage list hostonlyifs | grep "Name" | head -n1 | sed 's/Name:[ ]*//'`
echo "vboxmanage modifyvm $2 --nic1 hostonly --nictype1 82540EM --hostonlyadapter1 $hostonly_adapter_name"
vboxmanage modifyvm $2 --nic1 hostonly --nictype1 82540EM --hostonlyadapter1 $hostonly_adapter_name
