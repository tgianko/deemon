#!/bin/bash

set -e

if [ $# -ne 2 ]; then
    echo "usage: ./pamada </full/path/vm.vdmk> <vm-name>" 
    exit 1
fi

vmdk=$1
vm_name=$2
path=`echo $vmdk | grep -Eo '.*[/]'`
vm_file_name=$(basename $vmdk)
vm_file_name=`echo $vm_file_name | sed 's/\.vmdk//g'`
vdi="${path}${vm_file_name}-${vm_name}.vdi"

echo $vmdk
echo $vdi

./utils/vmdk_to_vdi.sh $vmdk $vdi
./polesno.sh $vdi $vm_name
