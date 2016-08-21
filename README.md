# Vilanoo Project

This is the code base for the Vilanoo project. The goal of this project is study
and detection of CSRF vulnerabilities.

## Components

 * [vilanoo2](vilanoo2/README.md) : HTTP/S proxy that intercepts browser requests.
 * [mosgi](mosgi/README.md): Server to collect Web Application execution traces, session data, and file I/O.
 * [zumka](vm-setup-scripts/README.md) : Tools to instrument VM (bitnami + vbox only)

## External components
 
 * [Selenium IDE](http://www.seleniumhq.org/download/): Tool to capture 
 user-generated Selenese HTML
 * [selenese-runner-java](https://github.com/tgianko/selenese-runner-java/tree/newfeat/interactive): 
 Tool to run Selenese HTML. This is a forked version by Giancarlo which feature
 a new option `--interactive` or `-i` to execute Selenese HTML step-by-step
 upon user input.

## License
  TBD

# Installation

Requirements and installation of internal component are [here](INSTALL.md).
For the external ones, please refer to the documentation of each project.

*Note*: A standalone .jar file of the interactive selenese-runner is in our 
repository.

# Quick start

боже мой - after successfully installing everything.

This is a quickstart guide to instrument a VM and use our toolset. 

## Step 1 - zumka and VM instrumentation

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


## Step 2 - mosgi + vilanoo2 to extract dynamic traces

Mosgi and Vilanoo2 work together. At the moment you will need to run first mosgi
and then vilanoo2. The other way around won't work.

The first step is to start mosgi: 

```
cd mosgi/src
./run.sh -h
```

Current version of mosgi is tough like Siberian winter and does not have default
parameter yet. All parameters are mandatory so, please, take your time and get
them right. 

It is likely that you will use the following command line:

```
./vilanoo/mosgi/run.sh -x /tmp/ -P /opt/bitnami/php/tmp/ -p 9292 -i 127.0.0.1 -t 192.168.56.101 -r root -c bitnami -s /path/to/db/
```

After that, Mosgi is up and running waiting for incoming connections at localhost
port 9292. 

Now, run vilanoo2 (to intercept also HTTPS request, please read [this](vilanoo2/src/README.md):

```
cd vilanoo2/
./vilanoo2.py -s $path_to_your_sqlitedb
```

# Step 3 - Run VM, Selenium IDE + Selenese Runner, and tests

Run the virtual machine and configure you browser or the testing tool to use 
127.0.0.1:8080 as a proxy. 

A guide to capture user traces and selenese runner is [here](vilanoo/docs/USER_TRACE.md)

# Step 4 - Database

HTTP requests, SQL query, xdebug traces, session data, and file I/O are stored
in the SQL Lite DB.


**Any steps that do not work or are not sufficiently discribed are a bug and should be
made a (seperate) ticket for us to fix.**