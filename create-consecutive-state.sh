#!/bin/bash


if [ $# -ne 6 ]; then
    echo "usage: ./create-consecutive-state.sh <vm-name> <vm-ip> <start-state> <test-case> <end-state> <firefox-instance>"
    exit 1
fi


vm_name=$1
vm_ip=$2
base_url="http://${vm_ip}"
start_state=$3
test_case=$4
end_state=$5
firefox_instance=$6
selenese_runner="./selenese-runner/selenese-runner.jar"

echo "My mission should I choose to accept it is:"
echo "starting ${vm_name} from state ${start_state}"
echo "executing ${test_case} with base url ${base_url}"
echo "finishing by taking snapshot ${end_state}"


if vboxmanage list vms | grep --quiet "\"${vm_name}\""; then
    
    if vboxmanage list runningvms | grep --quiet "\"${vm_name}\""; then
        echo "I am sorry, Dave. I am afraid I cannot do that"
	echo "test vm ${vm_name} is currently running - shut down before trying again"
	exit 1
    else
	echo `vboxmanage snapshot ${vm_name} restore ${start_state}`
	echo `vboxmanage startvm ${vm_name}`
    fi
    
else
    echo "machine ${vm_name} is unknown"
    exit 1
fi


java -jar ${selenese_runner} --firefox ${firefox_instance} --baseurl ${base_url} --height 2048 --width 2048 --set-speed 8000  ${test_case}


vboxmanage snapshot ${vm_name} take ${end_state}
vboxmanage controlvm ${vm_name} poweroff


echo "I love it when a plan comes together - success"
exit 0
