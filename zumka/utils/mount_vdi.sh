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

#This file mounts .vdi and dismounts them again


set -e

function waitForNbd {
	while [ ! -b /dev/nbd0p1 ]; do
		echo "Waiting for /dev/nbd0p1 to appear..."
		sleep 1
	done
}


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
	waitForNbd # We need this because the qemu-nbd command seems to return before the device is actually available
	mount -o loop /dev/nbd0p1 $3

	echo "mount success";;
    
    "--dismount")

	#echo "dismounting $2"
	
	umount $2 # --dismount is used in a different run where $2 should have the same value as $3 in --mount run
	qemu-nbd -d /dev/nbd0
	sleep 5
	rmmod nbd

	echo "dismount successfull";;
esac
