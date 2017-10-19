# ZUMKA

The zumka part of the toolchain is a collection of bash scripts that help setting up and installing a virtual
machine in virtualbox. 

## pamada.sh

This is the main script (and also the one used in `RUN_TEST.md`) it is basically a wrapper of `util/vmdk_to_vdi` and `polesno.sh` and calls them
in that order as `polesno.sh` expects an `.vdi` to work with. The finished result on running this script is an installed VM with the name
given as `<vm-name>`

``./pamada.sh </full/path/to/vm.vdmk> <vm-name>``

## polesno.sh

This is the essential script that does most of the heavy lifting to configure and install a VM. It checks whether a vm with `<vm-name>` already exists, adapt the
VM image to suite our needs by calling `./bitnami_setup_vm_harddrive.sh`, installing the VM by calling `./vbox_configure_vm.sh`, and finally
starting the VM to extract the assinged IP and create the snapshot `virgin-state` of the running vm. It shuts down the VM before exiting.

``./polesno.sh </full/path/to/vm.vdi> <vm-name>``

## bitnami_setup_vm_harddrive.sh

This script adapts the given `.vdi` bitnami image to our needs. In case a different VM type is to be used this script has to be adapted/swapped out. This provides
 modularity for adaptation in different contexts.

This script mounts the given `.vdi` and installs a small startup bashscript that transmitts the IP of the started VM to the `<host-ip>` on `<host_com_port>`. It then
swaps the `/etc/shadow` for a previously prepared one stored in our repository to ensure that we always have the right passwords. It then enables ssh as well as
root login. Finally it adapts the `php.ini` to activate xdebug usage and it deactivates apache `mod_pagespeed` before dismounting the `.vdi` and exiting.

``./bitnami_setup_vm_harddrive.sh </path/to/vm.vdi> <host_ip> <host_com_port>``

## vbox_configure_vm.sh

This script installs the (previously) prepared `.vdi` in virtualbox under the name of `<vm-name>` using the `vboxmanage` bash interface.

``./vbox_configure_vm.sh </path/to/vm.vdi> <vm-name>``
