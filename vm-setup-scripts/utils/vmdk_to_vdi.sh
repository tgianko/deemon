#this file converts a SINGLE vdmk file to a vdi
#author: Simon Koch <s9sikoch@stud.uni-saarland.de>

#!/bin/bash

file_extension=`echo "$1" | awk -F'[.]' '{print $NF}'`


if [ $# -neq 2 ]; then
    echo "./vmdk_to_vdi.sh </path/to/file.vmdk> </path/to/new/file.vdi>"
    exit 0
fi


if [ "$file_extension" != "vmdk" ]; then
    echo "ERROR: can only convert vmdk files"
    echo "conversion failed"
    exit 1
fi

echo "starting to clone vmdk file"
vboxmanage clonehd --format vdi $1 $2
echo "deleting old vmdk file"
rm $1
echo "finished conversion"
exit
