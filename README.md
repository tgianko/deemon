# Deemon Project

This is the code base for the Deemon project. The goal of this project is study and detection of CSRF vulnerabilities.

## Components

This project consists in a number of tools that are chained in a variety of ways. It also uses a number of existing tools.

### Dynamic Trace Acquisition Toolchain

 * [zumka](zumka/README.md): Tools to instrument VM (bitnami + vbox only)
 * [vilanoo](vilanoo/src/README.md): HTTP/S proxy that intercepts browser requests.
 * [mosgi](mosgi/README.md): Server to collect Web Application *raw* execution traces, session data, and file I/O.
 * [rawtrace-analysis](rawtrace-analysis/README.md): A tool that extracts SQL traces, session data snapshots, and file I/O operations from raw traces of mosgi and vilanoo.


### The Deep Modeling Framework

Deep Modeling is our awesome framework that allows to model and detect vulnerabilities. Its home is [here](deep-modeling/README.md). Our framework is based on Neo4j and it composed of three type of tools:

 * [dbmanager](deep-modeling/README.md): currently, dbmanager can initiate and reset the Neo4J database. Dbmanager is also responsible to import the data acquired with dynamic trace acquisition tools to Neo4J.
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


## License
  TBD

# Installation

Requirements and installation of internal component are [here](./docs/INSTALL.md).
For the external ones, please refer to the documentation of each project.

*Note*: A standalone .jar file of the interactive selenese-runner is in our 
repository.

# Tutorials

We prepared a quick tutorial to get into the testing for CSRF vulnerabilities right away 
[here](./docs/RUN_TEST.md) as well as a more extensive documentation of each tool involved
[here](./docs/TOOLCHAIN_ELEMENTS.md).
