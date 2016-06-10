#!/bin/bash

if [ "$1" == "--help" ] || [ "$1" == "-h" ] || [ $# -ne 4 ]; then
    echo "This script does the configuration setup for bitnami machines"
    echo "it requires that a virtualbox host-only network is already"
    echo "set up. Thus, the host ip is known beforehand"
    echo ""
    echo "usage: ./script </path/to/vm.vdi> <database_type> <host_ip> <host_com_port>"
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

mount_point='/mnt/'
setup_dir=`pwd`
mysql_script='/databases/mysql.sh'
bitnami_tool_dir='opt/cispa/'
bitnami_startup_script='etc/rc.local'
bitnami_incoming_port='4444'
bitnami_outgoing_port='5555'
bitnami_database_port='6666'
target_host_ip=$3
send_ip_target_port=$4

function clean_exit {
    ./utils/mount_vdi.sh --dismount $mount_point;
    exit $1;
}


./utils/mount_vdi.sh --mount $1 $mount_point #mount vm

if [ $? -ne 0 ]; then
    clean_exit $?
fi

#copy needed tools
mkdir $mount_point$bitnami_tool_dir
cp `which socat` $mount_point$bitnami_tool_dir #copy socat
cp `which redir` $mount_point$bitnami_tool_dir #and redir in bitnami_tool_dir of guest machine


##general startup configuration configuration
echo '#!/bin/sh -e' > "${mount_point}${bitnami_startup_script}"

chmod +x $mount_point$bitnami_startup_script

#make iptables accept incoming connections
echo 'iptables -A INPUT -t filter -j ACCEPT' >> $mount_point$bitnami_startup_script

#reroute incoming traffic from $bitnami_incoming_port to bitnami_database_port
echo "/${bitnami_tool_dir}/redir --lport=${bitnami_incoming_port} --cport=${bitnami_database_port} &" >> $mount_point$bitnami_startup_script 

echo "ifconfig eth0 | grep 'inet addr' | sed 's/Bcast.*//' | sed 's/.*inet addr://' | nc $target_host_ip $send_ip_target_port" >> $mount_point$bitnami_startup_script

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
echo "xdebug.collect.params=4"                                          >> "${mount_point}/opt/bitnami/php/etc/php.ini"
echo "xdebug.trace_format=1"                                            >> "${mount_point}/opt/bitnami/php/etc/php.ini"
echo "xdebug.collect_return=1"                                          >> "${mount_point}/opt/bitnami/php/etc/php.ini"
echo "xdebug.collect_assignments=1"                                     >> "${mount_point}/opt/bitnami/php/etc/php.ini"
echo "xdebug.trace_options=0"                                           >> "${mount_point}/opt/bitnami/php/etc/php.ini"
echo "xdebug.trace_output_dir=/tmp/"                                    >> "${mount_point}/opt/bitnami/php/etc/php.ini"

#database specific configuration
case $2 in
    "mysql") sh ${setup_dir}${mysql_script} ${mount_point} ${bitnami_tool_dir} ${target_host_ip} ${bitnami_outgoing_port} ${bitnami_startup_script} ${bitnami_database_port};;
    *) echo "$2 is an unknown database type"
       clean_exit 1;;
esac

clean_exit 0








