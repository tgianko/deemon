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

if [ "$1" == "--help" ] || [ "$1" == "-h" ] || [ $# -ne 3 ]; then
    echo "This script does the configuration setup for bitnami machines"
    echo "it requires that a virtualbox host-only network is already"
    echo "set up. Thus, the host ip is known beforehand"
    echo ""
    echo "usage: ./bitnami_setup_vm_harddrive.sh </path/to/vm.vdi> <host_ip> <host_com_port>"
    echo ""
    echo "currently supported database types:"
    echo "+ mysql"
    echo ""
    exit 1
fi

vm_file_extension=`echo "$1" | awk -F'[.]' '{print $NF}'`

if [ "$vm_file_extension" != "vdi" ]; then
    echo "ERROR: can only work with .vdi files!"
    echo "hint: use ./utils/vmdk_to_vdi.sh to convert the file"
    exit 1
fi

mount_point='/mnt'
setup_dir=`pwd`
mysql_script='/databases/mysql.sh'
bitnami_tool_dir='opt/cispa/'
bitnami_startup_script='etc/rc.local'
bitnami_incoming_port='4444'
bitnami_outgoing_port='5555'
bitnami_database_port='6666'
target_host_ip=$2
send_ip_target_port=$3

function clean_exit {
    ./utils/mount_vdi.sh --dismount $mount_point;
    exit $1;
}


./utils/mount_vdi.sh --mount $1 $mount_point #mount vm

if [ $? -ne 0 ]; then
    clean_exit $?
fi

##general startup configuration configuration
echo '#!/bin/sh -e' > "${mount_point}/${bitnami_startup_script}"

chmod +x "$mount_point/$bitnami_startup_script"

#make iptables accept incoming connections
#echo 'iptables -A INPUT -t filter -j ACCEPT' >> "$mount_point/$bitnami_startup_script"

#reroute incoming traffic from $bitnami_incoming_port to bitnami_database_port
#echo "/${bitnami_tool_dir}/redir --lport=${bitnami_incoming_port} --cport=${bitnami_database_port} &" >> "$mount_point/$bitnami_startup_script" 

echo "ifconfig eth0 | grep 'inet addr' | sed 's/Bcast.*//' | sed 's/.*inet addr://' | nc $target_host_ip $send_ip_target_port" >> "$mount_point/$bitnami_startup_script"


#enabling a root password
cp ./bitnami_shadow "${mount_point}/etc/shadow"
chmod "u-x,g-x,o-wx" "${mount_point}/etc/shadow" 

#enabling ssh
mv "${mount_point}/etc/init/ssh.conf.back" "${mount_point}/etc/init/ssh.conf"

#enabling root login with root account
sed -i -- 's/without-password/yes/g' "${mount_point}/etc/ssh/sshd_config" 

#adding trace generation
echo ""                                                                 >> "${mount_point}/opt/bitnami/php/etc/php.ini"
echo "[XDebug]"                                                         >> "${mount_point}/opt/bitnami/php/etc/php.ini"
echo "zend_extension=\"/opt/bitnami/php/lib/php/extensions/xdebug.so\"" >> "${mount_point}/opt/bitnami/php/etc/php.ini"
echo "xdebug.auto_trace=1"                                              >> "${mount_point}/opt/bitnami/php/etc/php.ini"
echo "xdebug.collect_params=4"                                          >> "${mount_point}/opt/bitnami/php/etc/php.ini"
echo "xdebug.trace_format=1"                                            >> "${mount_point}/opt/bitnami/php/etc/php.ini"
echo "xdebug.collect_return=1"                                          >> "${mount_point}/opt/bitnami/php/etc/php.ini"
echo "xdebug.collect_assignments=1"                                     >> "${mount_point}/opt/bitnami/php/etc/php.ini"
echo "xdebug.trace_options=0"                                           >> "${mount_point}/opt/bitnami/php/etc/php.ini"
echo "xdebug.trace_output_dir=/tmp/"                                    >> "${mount_point}/opt/bitnami/php/etc/php.ini"
echo "xdebug.trace_output_name=xdebug"                                  >> "${mount_point}/opt/bitnami/php/etc/php.ini"
echo "xdebug.var_display_max_data=16000"                                   >> "${mount_point}/opt/bitnami/php/etc/php.ini"

#disabling apache mod_pagespeed
apache_config="${mount_point}/opt/bitnami/apache2/conf/httpd.conf"
sed -i 's/Include conf\/pagespeed.conf/#Include conf\/pagespeed.conf/g' $apache_config
sed -i 's/Include conf\/pagespeed_libraries.conf/#Include conf\/pagespeed_libraries.conf/g' $apache_config



clean_exit 0








