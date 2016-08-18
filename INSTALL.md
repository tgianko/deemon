# Installation

This project is a collection of tools written in bash, python, and list. 
This version requires you to get familiar with them and to use them in the
right order. We are planning to provide a single command line to handle all the 
operations from one interface.

This project is developed on Ubuntu 14.04 and Ubuntu 16.04 with Gnome3/Unity. 
Any dependency is relative to this OS as a baseline. We try to give version 
numbers for every tool used, though it is quite likely that other versions work
just fine, they are just intended to give a perfect system layout that is 
guaranteed to work.

All scripts are based on `/bin/bash` and work with relative paths. If there are
constant paths used please report them as a bug.

*Comment*: As a downside of this approach it it would be a very bad idea to
rename folders INSIDE the repo without great caution

Every script intended for direct usage can be used without parameter and reports
the correct usage. If this is not the case please report that as a bug.

Every script contains a short description at the beginning of the file to 
explain what the script should be used for. If this is not the case please 
report that as a bug.

# VirtualBox

We run web applications under test inside virtual box and we rely on `vboxmanage`
being in the system path/known to the environment.

We furthermore need to mount the `.vdi` file of the vm to do static 
modifications to the contained files. For the mounting the following system
tools are needed:

* `modprobe` (version 15)
* `qemu-nbd` (version 0.0.1)
* `mount`    (version 2.20.1)
* `umount`   (version 2.20.1)
* `rmmod`    (version 15)

Additionally, our scripts require:
* `netcat` for OpenBSD (traditional netcat will not work, see Issue #43)

#Python

We use python 2.7. No additional requirements so far. We use proxy2 as our 
current proxy (https://github.com/inaz2/proxy2) but we keep a version in this
very repository.  

# Sqlite3

Installing `sqlite3` and `libsqlite3-dev` should suffice.

# Common-Lisp 

The following programs are needed:
* SBCL (Steel Bank Common Lisp version 1.1.14)
* quicklisp (https://www.quicklisp.org/beta/)

After installing quicklisp (follow the tutorial given on the homepage), please
create inside `~/quicklisp/local-projects/` the symlink

    ln -s /path/to/vilanoo/mosgi/src/ mosgi 


*VERY IMPORTANT*: We also rely on a library that needs patches for our purposes. 
We established a external repository to handle this. Clone it into 
"~/quicklisp/local-projects/" before executing any further steps using lisp.

    git clone https://github.com/simkoc/cl-libssh2.git

If something breaks and the string "libssh2" appears, please update the 
repository before making a bug report. We are in contact with the main author 
that handles the repository used by quicklisp but it takes time to push changes, 
thus relying on our own repo to fit our needs is more convienient.