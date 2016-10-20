# Vilanoo Project

This is the code base for the Vilanoo project. The goal of this project is study and detection of CSRF vulnerabilities.

## Components

This project consists in a number of tools that are chained in a variety of ways. It also uses a number of existing tools.

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
 * Neo4j
 * py2neo

## License
  TBD

# Installation

Requirements and installation of internal component are [here](INSTALL.md).
For the external ones, please refer to the documentation of each project.

*Note*: A standalone .jar file of the interactive selenese-runner is in our 
repository.

# Tutorials

We prepared a number of tutorials to start using our toolchains. 

 1. Tutorial for the acquisition and analysis of dynamic traces is [here](docs/TRACE_ACQUISITION.md)
 2. Tutorial to use our deep modeling framework is [here](docs/DEEP_MODELING.md)

# Quick start

боже мой - after successfully installing everything.

This is a quickstart guide to instrument a VM and use our toolset. 

## Data acquisition

### Step 1 - zumka and VM instrumentation

Zumka documentation is [here](zumka/README.md).

### Step 2 - extraction and analysis of dynamic traces

The components used for this step are: vilanoo2, mosgi, and rawtrace-analysis. Tutorials are available [here](docs/TRACE_ACQUISITION.md).

## Deep Modeling

### Step 1 - Importing traces and Deep Model transformations

Documentation is [here](deep-modeling/README.md).

### Step 2 - Running Security Analyses and Tests 

TBD

# Tested Bitnami Machines

* abantecart         1.2.4-1    working
* cmsmadesimple      2.1.4-0    working
* conc1rete5         5.7.5.8-0  not working (vilanoo issue#63)
* dolibarr       3.9.1-1    working
* enanocms       1.1.8-8    working
* horde          5.2.14-1   working
* invoiceninja       2.5.2.2-0  working
* joomla             3.5.1-1    working
* magento            2.0.7-0    not working (zumka issue#107)
* magento            1.9    working 
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
