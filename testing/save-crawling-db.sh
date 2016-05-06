#!/bin/bash

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
