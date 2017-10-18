# Deemon Project

This is the code base of Deemon, a tool to detect CSRF in web applications. 
Deemon is an application-agnostic, automated framework designed to be used by 
developers and security analysts during the security testing phase of the 
software development life-cycle. The current version of Deemon supports 
PHP-based web applications that use MySQL databases.

Deemon has been used for the paper [Deemon: Detecting CSRF with Dynamic Analysis and Property Graphs](http://trouge.net/gp/papers/csrf_ccs2017.pdf) *by G. Pellegrino, M. Johns, S. Koch, M. Backes, and C. Rossow.*

### Bibtex:


```
@inproceedings{deemon2017,
  title={{\textsc{Deemon}: Detecting CSRF with Dynamic Analysis and Property Graphs}},
  author={Pellegrino, Giancarlo and Johns, Martin and Koch, Simon and Backes, Michael and Rossow, Christian},
  booktitle={{Proceedings of the 2017 ACM Conference on Computer and Communications Security}},
  year={2017},
  organization={ACM}
}
```


## Components

This project consists in a number of tools that are chained in a variety of ways. It also uses a number of existing tools:

 * [zumka](zumka/README.md): Tools to instrument VM (bitnami + vbox only)
 * [vilanoo](vilanoo/src/README.md): HTTP/S proxy that intercepts browser requests.
 * [mosgi](mosgi/README.md): Server to collect Web Application *raw* execution traces, session data, and file I/O.
 * [rawtrace-analysis](rawtrace-analysis/README.md): A tool that extracts SQL traces, session data snapshots, and file I/O operations from raw traces of mosgi and vilanoo.
 * [dbmanager](deep-modeling/README.md): The tool create a property graph of the web application. It imports dynamic traces and infers (1) finate-state machines, (2) data-flow models, and (3) data types. 
 * [testermanager](deep-modeling/README.md): The tool to generate tests to detect CSRF vulnerabilities
 * [csrf-test-runner](csrf-test-runner/README.md): The tool to execute tests against a web application


## External components
 
 Deemon relies on two external tools:
 
 * [Selenium IDE](http://www.seleniumhq.org/download/): (not included in Deemon) Tool to capture 
 user-generated Selenese HTML
 * [selenese-runner-java](https://github.com/tgianko/selenese-runner-java/tree/newfeat/interactive): (included in Deemon)
 Tool to run Selenese HTML. This is a forked version by Giancarlo which feature
 a new option `--interactive` or `-i` to execute Selenese HTML step-by-step
 upon user input.
 * [proxy2](https://github.com/inaz2/proxy2): (included in Deemon) HTTP/HTTPS proxy in a single
 python script.


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
