#This file mounts .vdi and dismounts them again
#author: Simon Koch <s9sikoch@stud.uni-saarland.de>

#!/bin/bash


if [ "$(id -u)" != "0" ]; then
    echo "mound_vdi.sh has to be run as root"
    exit 1
fi

case $1 in
    "--mount")

	echo "trying to mount virtual disk image to $3"
	
	filename=$(basename "$2")
	extension="${filename##*.}"

	#echo $extension
	
	if [ "$extension" != "vdi" ]; then
	    echo "ERROR: this script can only mount .vdi files"
	    echo "mounting failed"
	    exit 1
	fi
	
	modprobe nbd
	qemu-nbd -c /dev/nbd0 $2
	mount -o loop /dev/nbd0p1 $3

	echo "mount success";;
    
    "--dismount")

	#echo "dismounting $2"
	
	umount $2
	qemu-nbd -d /dev/nbd0
	rmmod nbd;;

	#echo "dismount successfull";;
esac
