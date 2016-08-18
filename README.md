# Vilanoo Project

This is the code base for the Vilanoo project. The goal of this project is study and detection of CSRF vulnerabilities.

## Components

 * [vilanoo2](vilanoo2/README.md) : HTTP/S proxy that intercepts browser requests.
 * [mosgi](mosgi/README.md): Scripts for collecting server-side execution trace, extract session data, and file I/O.
 * [zumka](vm-setup-scripts/README.md) : Scripts for setting up the vm for usage with the proxy

## License
  TBD

# Installation

Requirements and installation are [here](INSTALL.md)

# Quick start

боже мой - after successfully installing everything.

This is a quickstart guide to instrument a VM and use our toolset. 

## (step 1) `zumka` - VM Instrumentation

zumka supports only bitnami images with PHP and MySQL. This step is executed 
only once per VM. 

If you have a `.vmdk` image files use:


    cd /path/to/vilanoo/vm-setup-scripts/
    ./pamada.sh </full/path/vm.vmdk>


else (the vm is in the form of a `.vdi` file):


    cd /path/to/vilanoo/vm-setup-scripts/
    ./polesno.sh </full/path/to/vm.vdi>


If this yields any errors please give a bug report consisting of:
* exact command given
* full output from start to crash of the execution

The vm should be up and running at the end of the script and the ip
of the vm is printed in the end. The start-up of the vm will take
a couple minutes (see the corresponding issue).

**IMPORTANT**: If at some step something did not work out and the problem
is fixed it is important to do a **COMPLETE** reset. Basically deleting the
vm-folder and using a fresh version. No relative restart is possible and
just ends in even more weird and confusing error messages.


### Getting the interception running

We have to start all the proxy parts in the correct order:
* mosgi (the lisp stuff - don't worry there is a cmd script for that)
* vilanoo-proxy  (don't worry there is a cmd script for that)

The first step is to start `mosgi`: 

    ./vilanoo/mosgi/run.sh

all flags are listed and explained using the `-h` flag. If not mentioned otherwise a command is
mandatory. All flags are mandatory.

For example:

    ./vilanoo/mosgi/run.sh -x /tmp/ -P /opt/bitnami/php/tmp/ -p 9292 -i 127.0.0.1 -t 192.168.56.101 -r root -c bitnami -s /path/to/db/ ?!? <- can you explain me this?


The second step is to start `vilanoo`: 

    python ./vilanoo/proxy2/src/vilanoo2.py <PORT-TO-LISTEN-FOR-INCOMING>

Mosgi should (now) display, that a connection has been established.

As the last step we need to configure the browser to use our proxy, this can be
done however fits the used browser.
Finally, we can access the vm, using the ip of the machine, to access the webpage we are
interested in (that is on the bitnami machine of course).


**Any steps that do not work or are not sufficiently discribed are a bug and should be
made a (seperate) ticket for us to fix.**