#!/bin/bash

if [ "$1" == "--help" ] || [ "$1" == "-h" ] || [ $# -ne 2 ]; then
    echo "This script does the configuration setup for bitnami machines"
    echo ""
    echo "usage: ./script /path/to/vm.vmdk database_type"
    echo ""
    echo "currently supported database types:"
    echo "+ mysql"
    echo ""
    exit
fi

mount_point='/mnt/'
setup_dir=`pwd`
mysql_script='/databases/mysql.sh'
bitnami_tool_dir='opt/cispa/'
bitnami_startup_script='etc/rc.local'
bitnami_incoming_port='4444'
bitnami_outgoing_port='5555'
bitnami_database_port='6666'
target_host_ip='192.168.72.1'

function clean_exit {
    vmware-mount -d $mount_point;
    exit;
}

#configure vm to use NAT
sed -i 's/Ethernet0.connectionType = .*/Ethernet0.connectionType = "nat"/' "${1%.*}.vmx"

vmware-mount $1 $mount_point #mount vm

#copy needed tools
mkdir $mount_point$bitnami_tool_dir
cp `which socat` $mount_point$bitnami_tool_dir #copy socat
cp `which redir` $mount_point$bitnami_tool_dir #and redir in bitnami_tool_dir of guest machine


##general startup configuration configuration
echo '#!/bin/sh -e' > $mount_point$bitnami_startup_script
chmod +x $mount_point$bitnami_startup_script

#make iptables accept incoming connections
echo 'iptables -A INPUT -t filter -j ACCEPT' >> $mount_point$bitnami_startup_script

#reroute incoming traffic from $bitnami_incoming_port to bitnami_database_port
echo "/${bitnami_tool_dir}/redir --lport=${bitnami_incoming_port} --cport=${bitnami_database_port} &" >> $mount_point$bitnami_startup_script 

#database specific configuration
case $2 in
    "mysql") sh ${setup_dir}${mysql_script} ${mount_point} ${bitnami_tool_dir} ${target_host_ip} ${bitnami_outgoing_port} ${bitnami_startup_script} ${bitnami_database_port};;
    *) echo "$2 is an unknown database type"
       clean_exit;;
esac

clean_exit








