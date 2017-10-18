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


create-timestamp() {
    local result=`date +"%Y%m%d%H%M"`
    echo "$result"
}

folder_string="${HOME}/.vilanoo/${1}/"

if [ -d ${folder_string} ]; then
    echo "there is already such a crawling folder"
else
    mkdir ${folder_string}
fi


currtime=`create-timestamp`
mv "${HOME}/.vilanoo/vilanoo.db" "${folder_string}/spiderun-${currtime}.db"
