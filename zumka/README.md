# Zumka

This tool is responsible to instrument and initialize VMs.

# Dependencies

 * virtual box ~5.02
   - vboxmanage
 * qemu-nbd

# Quick Start

zumka supports only bitnami images with PHP and MySQL. This step is executed 
only once per VM. 

If you have a `.vmdk` image files use:

```bash
cd zumka/
./pamada.sh  </full/path/vm.vdmk> <vm-name>
```

If you have a `.vdi` image file then use:

```bash
cd zumka/
./polesno.sh </full/path/vm.vdi> <vm-name>
```

If this yields any errors please open an issue consisting of:
* exact command given
* full output from start to crash of the execution

At this point, if everything worked correctly, the VM should be up and running.
At the end of the execution of the instrumentation scripts, you will see IP,
ports, and snapshot name. You can use the IP to connect to the VM for testing.

**IMPORTANT**: If at some step something did not work out and the problem
is fixed it is important to do a **COMPLETE** reset. Basically deleting the
vm-folder and using a fresh version. No relative restart is possible and
just ends in even more weird and confusing error messages.
