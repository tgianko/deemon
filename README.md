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

## Step 1 - `zumka` and VM Instrumentation

zumka supports only bitnami images with PHP and MySQL. This step is executed 
only once per VM. 

If you have a `.vmdk` image files use:

```bash
cd zumka/
./pamada.sh $vmdk_file
```

If you have a `.vdi` image file then use:

```bash
cd zumka/
./polesno.sh $vdi_file
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


## Step 2 - MOSGI + Vilanoo2 to extract dynamic traces

Mosgi and Vilanoo2 work together. At the moment you will need to run first mosgi
and then vilanoo2. The other way around won't work.

The first step is to start `mosgi`: 

```
cd mosgi/src
./run.sh -h
```

Current version of MOSGI is tough like Siberian winter and does not have default
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



Mosgi should (now) display, that a connection has been established.

As the last step we need to configure the browser to use our proxy, this can be
done however fits the used browser.
Finally, we can access the vm, using the ip of the machine, to access the webpage we are
interested in (that is on the bitnami machine of course).


**Any steps that do not work or are not sufficiently discribed are a bug and should be
made a (seperate) ticket for us to fix.**