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
