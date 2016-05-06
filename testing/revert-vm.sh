#!/bin/bash

vm_id=${1}
snapshot_name=${2}

vboxmanage controlvm ${vm_id} poweroff
vboxmanage snapshot ${vm_id} restore "${snapshot_name}"
vboxmanage startvm ${vm_id}
