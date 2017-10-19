#this file converts a SINGLE vdmk file to a vdi
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


file_extension=`echo "$1" | awk -F'[.]' '{print $NF}'`


if [ $# -ne 2 ]; then
    echo ""
    echo "usage: ./vmdk_to_vdi.sh </path/to/file.vmdk> </path/to/new/file.vdi>"
    echo ""
    exit 1
fi


if [ "$file_extension" != "vmdk" ]; then
    echo ""
    echo "ERROR: can only convert vmdk files"
    echo "conversion failed"
    echo ""
    exit 1
fi

echo "starting to clone vmdk file"
sudo vboxmanage clonehd --format vdi $1 $2
sudo chmod 666 $2
#echo "deleting old vmdk file"
#rm $1
echo "finished conversion - remember to delete old vmdk files"
exit
