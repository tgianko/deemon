# Deep Modeling

## Building a Deep Model

We assume that you have prepared already a vilanoo2 SQLite DB and a rawtrace-analysis SQLite DB. These two databases are imported in Neo4j with `dbmanager.py`.

`dbmanager.py` allows to initialize, reset, and import the above databases.

Make sure that you have properly installed Neo4j and that you have valid credentials. 

### Configuration

An example of database access configuration is in `deep-modeling/shared/config.py.example`. Copy it into `deep-modeling/shared/config.py`:

```bash
$ cp deep-modeling/shared/config.py.example deep-modeling/shared/config.py
```

Run your favourite text editor, open `deep-modeling/shared/config.py`, and put the appropriate values for Neo4j server hostname, username, and password.

### Init Neo4j DB

To initialize the Neo4j database type the following:

```bash
$ ./dbmanager.py init
```

### Reset Neo4j DB

To initialize the Neo4j database type the following:

```bash
$ ./dbmanager.py reset
```

### Import data

To import dynamic traces into Neo4j, you will need the vilanoo2 SQLite database, the rawtrace-analysis database, a project name, a session ID, and username. The project name can be, for example, the name of the web application. The session ID can be a number. If you import two traces for the same web application you may want to use two numbers in order to distinguish them. Finally, the username can be the name of the account that you used in the web application under test. 

To import all data type the following:

```bash
$ ./dbmanager.py import all $path_to_vilanoo_sqlite $path_to_mosgi_sqlite $path_to_rawtrace_sqlite $projname $session $user
```

You can import traces one by one. Have a look at the help page of dbmanager (`--help`).


## Running Analysis on a Deep Model

IMPORTANT: The current version of our framework assumes that you use the suffix `_S1` and `_S2` to distinguish between session 1 and session 2. This is ugly and we acknowledge that. But you have to deal with it until we find resources to improve usability :)

We can run a number of analysis on a deep model. Analyses may depend from each other in the sense that part of the output of one analysis (e.g., new nodes in the model) are needed for another one.

Below I am listing a working order of analysis. If you want to build a model follow this order. Details on the hidden dependencies may be shared soon :)

1. Dataflow (insert variables, vertical chains and backward selenese chains)
2. User Generated Chains (insert propagation types for variables. Only UG so far)
3. Intra-causality (causality between events, e.g., via Referer header)
4. Add abstract parse trees (so far, we have only SQL query abstraction)
5. Variable type inference (creation of abstract event and semantic+syntactic type inference)
6. Model inference (FSM creation)

Here is a mapping of the above analyses with `dbmanager` commands. If you want to build a model, you should run `dbmanager` using the commands below in the give order. Use `--help` to see the exact type of parameters:

* `dbmanager.py analysis all`: analyses number 1, 2, 3, and 4 (it is not really ALL)
* `dbmanager.py type`: analysis 5
* `dbmanager.py analysis inference`: analysis 6
