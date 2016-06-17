# Vilanoo Project

This is the code base for the Vilanoo project. The goal of this project is study and detection of CSRF vulnerabilities.

## Components

 * [vilanoo](vilanoo-proxy/README.md) : HTTP + data layer proxy
 * [zumka](vm-setup-scripts/README.md) : Scripts for setting up the vm for usage with the proxy
 * [mosgi](mosgi/README.md): Scripts for collecting server-side execution trace, extract session data, and file I/O
## License
  TBD

# HowTo [Installation/Usage]:

The whole project relies on multiple different technologies. Though we tried to provide a general interface
to hide that fact each part needs to be addressed independently. First we describe installation then 
usage (which is the easier part). We use the project on Ubuntu 14.04 with gnome3 and, thus, any dependency
is relative to this OS as a baseline. We will try to give version numbers for every tool used, though it
is quite likely that other versions will work just fine, they are just intended to give a perfect system
layout that is guaranteed to work.

All scripts are based on `/bin/bash` and work with relative paths. If there are constant paths used please 
report them as a bug.

*Comment*: As a downside of this approach it it would be a very bad idea to rename folders INSIDE the repo without
great caution

Every script intended for direct usage can be used without parameter and reports the correct usage.
If this is not the case please report that as a bug.

Every script contains a short description at the beginning of the file to explain what the script should be
used for. If this is not the case please report that as a bug.

Running the whole program will create a folder `~/.vilanoo/` which will contain project relevant information.
This also means that the database used is contained in that folder and called "./vilanoo.db". Any flag
requesting this parameter provide a path to that database.

## Installation

### VirtualBox
We use VirtualBox as it provides a clean cmd interface and the shell scripts rely on `vboxmanage` being
in the system path/known to the environment.

We furthermore need to mount the `.vdi` file of the vm to do static modifications to the contained files.
This has been tested only on `Bitnami-OpenCart (2.1.0.2.0)` so far. For the mounting the following system
tools are needed:
* `modprobe` (version 15)
* `qemu-nbd` (version 0.0.1)
* `mount`    (version 2.20.1)
* `umount`   (version 2.20.1)
* `rmmod`    (version 15)

Additionally the bitnami machines lack the tools:
* `socat` (version 1.7.2.3)
* `redir` (version 2.2.1)
Those are copied onto the machine from the host system and thus need to be available and detectable using the
`which` command.



### Python
We are using python2.7 and the needed dependencies are listed in `./vilanoo-proxy/README.md`.


### Sqlite3
Installing `sqlite3` and `libsqlite3-dev` should suffice.


### Common-Lisp 
The following things are needed:
* SBCL (Steel Bank Common Lisp version 1.1.14)
* quicklisp (https://www.quicklisp.org/beta/)

After installing quicklisp (follow the tutorial given on the homepage) set the symlink

        ln -s /path/to/vilanoo/mosgi/src/ mosgi 

in `./quicklisp/local-projects/`


## Usage
боже мой - after successfully installing everything

### Setting up the vm (this is only required once!)

If the vm is in form of `.vmdk` files use:

         ./vilanoo/vm-setup-scripts/pamada.sh </full/path/vm.vmdk> mysql

else (the vm is in the form of a `.vdi` file):

         ./vilanoo/vm-setup-scripts/polesno.sh </full/path/to/vm.vdi> mysql

If this yields any errors please give a bug report consisting of:
* exact command given
* full output from start to crash of the execution

The vm should be up and running at the end of the script and the ip
of the vm is printed in the end. The start-up of the vm will take
a couple minutes (see the corresponding issue).

**IMPORTANT**: If at some step something did not work out and the problem
is fixed it is important to do a **COMPLETE** reset. Basically deleting the
vm-folder and using a fresh version. No relative restart is possible and
will just end in even more weird and confusing error messages.


### Getting the interception running

We have to start all the proxy parts in the correct order:
* mosgi (the lisp stuff - don't worry there is a cmd script for that)
* vilanoo-proxy  (don't worry there is a cmd script for that)

The first step is to start `mosgi`: 

    ./vilanoo/mosgi/run.sh

all flags are listed and explained using the `-h` flag. If not mentioned otherwise a command is
mandatory.

The second step is to start `vilanoo`: 

        ./vilanoo/vilano-proxy/src/ <bind_proxy_address> <bind_proxy_port> <mysql_proxy_ip> <mysql_proxy_port> <mysql_server_host> <mysql_server_port>

Mosgi should (now) display, that a connection has been established.

As the last step we need to configure the browser to use `vilano-proxy` as our proxy.
Then we can access the vm using the ip of the machine to access the webpage we are
interested in (that is on the bitnami machine of course)


**Any steps that do not work or are not sufficiently discribed are a bug and should be
made a (seperate) ticket for us to fix.**