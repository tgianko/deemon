#!/bin/bash

if [ $# -ne 2 ]; then
    echo "usage: ./pamada </full/path/vm.vdmk> <database_type>"
    exit 1
fi

vmdk=$1
path=`echo $vmdk | grep -Eo '.*[/]'`
vm_file_name=$(basename $vmdk)
vm_file_name=`echo $vm_file_name | sed 's/\.vmdk//g'`
vdi="${path}${vm_file_name}.vdi"
database=$2

echo $vmdk
echo $vdi

./utils/vmdk_to_vdi.sh $vmdk $vdi
./polesno.sh $vdi $database
