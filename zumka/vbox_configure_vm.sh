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
# along with Deemon.  If not, see <http://www.gnu.org/licenses/>

set -e

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


#set the hardware we want to give the vm
vboxmanage modifyvm $2 --memory 1024 
vboxmanage modifyvm $2 --cpus 2           

#set up the network adapter
hostonly_adapter_name=`vboxmanage list hostonlyifs | grep "Name" | head -n1 | sed 's/Name:[ ]*//'`
echo "vboxmanage modifyvm $2 --nic1 hostonly --nictype1 82540EM --hostonlyadapter1 $hostonly_adapter_name"
vboxmanage modifyvm $2 --nic1 hostonly --nictype1 82540EM --hostonlyadapter1 $hostonly_adapter_name
