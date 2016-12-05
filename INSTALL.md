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

# Python

We use python 2.7. We use proxy2 as our 
current proxy (https://github.com/inaz2/proxy2) but we keep a version in this
very repository. Utilities needed are:

* `virtualenv` 

# Sqlite3

Installing `sqlite3` and `libsqlite3-dev` should suffice.

# Common-Lisp 

The following programs are needed:
* SBCL (Steel Bank Common Lisp version 1.1.14)
* quicklisp (https://www.quicklisp.org/beta/)

After installing quicklisp (follow the tutorial given on the homepage), please
create inside `~/quicklisp/local-projects/` the symlink

```bash
ln -s /path/to/vilanoo/mosgi/src/ mosgi 
ln -s /path/to/vilanoo/rawtrace-analysis/src/ analyzer
```

*VERY IMPORTANT*: We also rely on a library that needs patches for our purposes. 
We established a external repository to handle this. Clone it into 
"~/quicklisp/local-projects/" before executing any further steps using lisp.

    git clone https://github.com/simkoc/cl-libssh2.git

If something breaks and the string "libssh2" appears, please update the 
repository before making a bug report. We are in contact with the main author 
that handles the repository used by quicklisp but it takes time to push changes, 
thus relying on our own repo to fit our needs is more convienient.

# Neo4j

We are using neo4j v.3.0.6

# py2neo

The data model of our deep-modeling framework is based on a mapping between neo4j property graphs and python objects. We use the Object Graph Mapping extension of py2neo for that. However, for a limitation of py2neo.ogm on heterogeneous relationships (see [Github Issue 573](https://github.com/nigelsmall/py2neo/issues/573)), we forked the GitHub project here [tgianko/py2neo](https://github.com/tgianko/py2neo) and fixed it. 

To use our version DO NOT USE `pip install py2neo`, but instead, create and use a python virtual environments. 

To create a virtual environment follows these steps:

```bash
$ cd deep-modeling/

$ virtualenv py2neodev_env
Running virtualenv with interpreter /usr/bin/python2
New python executable in /home/gianko/Desktop/Projects/vilanoo/deep-modeling/py2neodev_env/bin/python2
Also creating executable in /home/gianko/Desktop/Projects/vilanoo/deep-modeling/py2neodev_env/bin/python
Installing setuptools, pkg_resources, pip, wheel...done.
```

This will create a virtual environment with basic packages. 

To install our modified version of py2neo, activate the virtual environment and then install the package as follows:

```bash
$ source py2neodev_env/bin/activate

(py2neodev_env) $ pip install git+https://github.com/tgianko/py2neo.git#egg=py2neo

Collecting py2neo from git+https://github.com/tgianko/py2neo.git#egg=py2neo
  Cloning https://github.com/tgianko/py2neo.git to /tmp/pip-build-61HXMR/py2neo
Installing collected packages: py2neo
  Running setup.py install for py2neo ... done
Successfully installed py2neo-3.1.2
```

This will install py2neo within the virtual environment. From this point on, when using any of the deep-modeling tools, please activate the virtual environment before running the script. If you do not do that, python will not use our version, but instead the system one.

# Further dependencies

One additionally needs `libssh2-1-dev` to be installed. Also `~/.ssh` has to be existent.