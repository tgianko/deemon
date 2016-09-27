# Vilanoo Project

This is the code base for the Vilanoo project. The goal of this project is study
and detection of CSRF vulnerabilities.

## Components

### Dynamic Trace Acquisition Toolchain

 * [zumka](zumka/README.md): Tools to instrument VM (bitnami + vbox only)
 * [vilanoo2](vilanoo2/src/README.md): HTTP/S proxy that intercepts browser requests.
 * [mosgi](mosgi/src/README.md): Server to collect Web Application *raw* execution traces, session data, and file I/O.
 * [rawtrace-analysis] (rawtrace-analysis/src/README.md): A tool that extracts SQL traces, session data snapshots, and file I/O operations from raw traces of mosgi and vilanoo2.
 
### The Deep Modeling Framework  (UNDER DEVELOPMENT)

Deep Modeling is our awesome framework that allows to model and detect vulnerabilities. Its home is [here](deep-modeling/README.md). Our framework is based on Neo4j and it composed of three type of tools:

 * [dbmanager](deep-modeling/dbmanager.py): currently, dbmanager can initiate and reset the Neo4J database. Dbmanager is also responsible to import the data acquired with dynamic trace acquisition tools to Neo4J.
 * *analysis* algorithms: these algorithms can be
   * clustering algorithms
   * model inference algorithms
   * data flow analysis algorithms
   * vulnerability detection algorithms (in cases where the presence of the vulnerability can be verified in the model)
   * test case generation algorithms (in cases where the presence of the vulnerability need to be verified with a test against the real system)


## External components
 
 * [Selenium IDE](http://www.seleniumhq.org/download/): Tool to capture 
 user-generated Selenese HTML
 * [selenese-runner-java](https://github.com/tgianko/selenese-runner-java/tree/newfeat/interactive): 
 Tool to run Selenese HTML. This is a forked version by Giancarlo which feature
 a new option `--interactive` or `-i` to execute Selenese HTML step-by-step
 upon user input.

## Requirements

 * `tmux`

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


## Step 2 - mosgi + vilanoo2 + dyntrace to extract dynamic traces

### Extraction of **raw** dynamic traces

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
./vilanoo/mosgi/run.sh -x /tmp/ -P /opt/bitnami/php/tmp/ -p 9292 -i 127.0.0.1 -t 192.168.56.101 -r root -c bitnami -s $path_to_your_mosgi_sqlitedb
```

After that, Mosgi is up and running waiting for incoming connections at localhost
port 9292. 

Now, run vilanoo2 (to intercept also HTTPS request, please read [this](vilanoo2/src/README.md):

```bash
cd vilanoo2/
./vilanoo2.py -s $path_to_your_vilanoo_sqlitedb
```

### Analysis of raw traces.

Vilanoo2 and Mosgi generate **raw** traces. The output of these two tools are two SQLite3 databases. Starting from these databases, 
dyntrace extract traces with SQL operations, session data snapshots, and disk operations.

```bash
cd rawtrace-analysis/src/
./run-analyzer.sh -m $path_to_your_mosgi_sqlitedb -v $path_to_your_vilanoo_sqlitedb -d $path_to_your_rawtraceanalysis_sqlitedb -S ../../data/DBSchema.sql
```

This will create a new SQLiteDB3 `$path_to_your_rawtraceanalysis_sqlitedb` from the analysis on `$path_to_your_mosgi_sqlitedb` and `$path_to_your_vilanoo_sqlitedb`.

## Step 3 - Run VM, Selenium IDE + Selenese Runner, and tests

Run the virtual machine and configure you browser or the testing tool to use 
127.0.0.1:8080 as a proxy. 

A guide to capture user traces and selenese runner is [here](selenese-runner/README.md)

## Step 4 - Database

HTTP requests, SQL query, xdebug traces, session data, and file I/O are stored
in the SQL Lite DB.


**Any steps that do not work or are not sufficiently discribed are a bug and should be
made a (seperate) ticket for us to fix.**


# Tested Bitnami Machines

* abantecart         1.2.4-1    working
* cmsmadesimple      2.1.4-0    working
* conc1rete5         5.7.5.8-0  not working (vilanoo issue#63)
* dolibarr       3.9.1-1    working
* enanocms       1.1.8-8    working
* horde          5.2.14-1   working
* invoiceninja       2.5.2.2-0  working
* joomla             3.5.1-1    working
* magento            2.0.7-0    working 
* mautic             1.4.1-0    not working (vilanoo issue#64)
* modx           2.4.4pl-1  not working (vilanoo issue#65)
* opencart       2.1.0.2-2  working
* oxid           4.9.8-0    working
* prestashop         1.6.1.2-1  working
* roundcube      1.1.4-3    working
* silverstripe       3.4.0-0    not working (setup issue#66)
* simpleinvoices         2013.beta.8-4  working
* typos3             8.1.2-0    working
* xoops          2.5.7.2-2  working
