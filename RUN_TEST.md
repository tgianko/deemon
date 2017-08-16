This guide is meant to give a complete run from having nothing up to the point of running a csrf test.

# Installation

Installation is covered [here](./INSTALL.md).

# Setting Up a VM

First step is to download a `.vdmk` from bitnami that conforms to our requirements (LAMP Stack) and that we want to test. We Unzip the file and switch into the `{deemon_folder}/zumka` folder. Run

```
/pamada.sh <full/path/to/vdmk> <name_of_vm>
```

When the script finishes it tells the setup details (i.e. IP) of the machine for further usage.

## Example

```
./pamada.sh /home/user/csrf/vms/bitnami-opencart-2.1.0.1-1-ubuntu-14.04/bitnami-opencart-2.1.0.1-1-ubuntu-14.04.vmdk vilanoo-opencart
```

# Run Trace Acquisition

We implemented a wrapper bash script that does all the steps necessary to run the basic trace acquisition needed to generate csrf tests.

```
./run-test.sh <vm-name> <vm-ip> <test-name> <start-state-name> <selenese-test-file> <firefox-instance> <mosgi-port> <vilanoo-port>
```

`run-test.sh` runs the specified test automatically and creates the resulting raw databases in the `${HOME}/.vilanoo/` folder.

You have to run this acquisition twice for the same selenese test script (we call them S1 and S2) so it is possible to later differentiate between randomly set variables and constant.

## Example

```
./run-test.sh vilanoo-opencart 192.168.56.101 css-cleanup-S1 virgin-state /home/user/csrf/selenese-testcases/opencart/opencart-TS-11-admin-login_and_change_product_price.html /home/user/csrf/firefox/firefox 8787 9898

./run-test.sh vilanoo-opencart 192.168.56.101 css-cleanup-S2 virgin-state /home/user/csrf/selenese-testcases/opencart/opencart-TS-11-admin-login_and_change_product_price.html /home/user/csrf/firefox/firefox 8787 9898
```

# Pre Processing Traces

After acquisition we need to do preprocessinge on the generated databases before the model can be generated. The previous step generated two sqlite databases in `~/.vilanoo/`. Both database names are based on the test-name and one has the appendix `vilanoo` the other `mosgi. To perform this step we need to switch into the `${deemon_folder}/rawtrace-analysis/src/` folder and run 

```
run-analyzer.sh -m <mosgi-db> -v <vilanoo-db> -d <analyzed-db> -S <DBSchema>
``` 

To name the resulting database we follow the practice of calling the database exactly the same but the appendix being `analyzed`. We also create the database at the same folder as the other databases so all databases are neatly stored at the same folder.

## Example

```
./run-analyzer.sh -m /home/user/.vilanoo/css-cleanup-S1-201708131721-mosgi.db -v /home/user/.vilanoo/css-cleanup-S1-201708131721-vilanoo.db -d /home/user/.vilanoo/css-cleanup-S1-201708131721-analyzed.db -S ../../data/DBSchema.sql

./run-analyzer.sh -m /home/user/.vilanoo/css-cleanup-S2-201708131726-mosgi.db -v /home/user/.vilanoo/css-cleanup-S2-201708131726-vilanoo.db -d /home/user/.vilanoo/css-cleanup-S2-201708131726-analyzed.db -S ../../data/DBSchema.sql
```

# Generating the Deep Model

Now that we finally collected and generated all the needed data we can proceed in creating the Deep Model on which the csrf detection is based. For this we switch into the `${deemon_folder}/deep-modeling/` folder and run 

```
./dbamanger import all <vilanoo-db> <mosgi-db> <analyzed-db> <project> <session-name> <user>
``` 

The whole execution may take some time depending on the size of the trace. 

*Note:* In case we were creating the database for the first time we need to run

```
./dbmanager reset

./dbmanager init
```

to get rid of old data and initialize keys. This is however NOT MANDATORY if this has to be done at least once before.


## Example


```
./dbmanager.py import all /home/user/.vilanoo/css-cleanup-S1-201708131721-vilanoo.db /home/user/.vilanoo/css-cleanup-S1-201708131721-mosgi.db /home/user/.vilanoo/css-cleanup-S1-201708131721-analyzed.db opencart css_cleanup_S1 admin

./dbmanager.py import all /home/user/.vilanoo/css-cleanup-S2-201708131726-vilanoo.db /home/user/.vilanoo/css-cleanup-S2-201708131726-mosgi.db /home/user/.vilanoo/css-cleanup-S2-201708131726-analyzed.db opencart css_cleanup_S2 admin
```

# Analyzing the Deep Model

After inserting all the data we have to run analysis on them to extract/infer the needed information. For this we run

```
./dbmanger analysis all <projname> <session-name> <user>`
```

Following this we still need to do two more analysis steps before we can start with the test extraction 

```
./dbmanager type all <project> <operation>
```` 

and 

```
./dbmanager.py analysis inference <project> <session-name> <user>
```

## Example

```
./dbmanager.py analysis all opencart css_cleanup_S1 admin

./dbmanager.py analysis all opencart css_cleanup_S2 admin

./dbmanager.py type all abantecart css_cleanup

./dbmanager.py analysis inference opencart css_cleanup_S1 admin

./dbmanager.py analysis inference opencart css_cleanup_S2 admin
```

# Extracting CSRF Test Candidates

Now that we have the whole model we can start extracting candidates for testing for csrf vulnerabilities.
There are two types of tests. One test aims to probe requests that seem to be unprotected by any csrf token 

```
./testmanager.py tgen not_protected <project> <operation> <csrf-db>
```

and one to generate tests for presumably protected requests 

```
./testmanager.py tgen protected <project> <operation> <csrf-db>
```

## Example

```
./testermanager.py tgen protected opencart css_cleanup /home/user/.vilanoo/css-opencart-csrf-protected.db

./testermanager.py tgen not_protected opencart css_cleanup /home/user/.vilanoo/css-opencart-csrf-not-protected.db
```

# Executing the CSRF Tests

We now have a sqlite database containing the to-test urls for either protected or non-protected http requests. To execute a stored test we wrote a small handy script in the base folder called `./run-test-runner.sh` that does all the setup and initialization that is needed. It also creates the corresponding databases in `${HOME}/.vilanoo/`.

```
./run-test-runner.sh <vm-name> <vm-ip> <vm-state> <login-tc> <wait-sec> <firefox> <csrf-db> <test-id>
```

## Example

```
./run-test-runner.sh vilanoo-opencart 192.168.56.101 virgin-state /home/user/csrf/selenese-testcases/abantecart/Abantecart_12_user_logs_in.html 10 /home/user/csrf/firefox/firefox /home/user/.vilanoo/css-opencart-csrf-protected.db 0

./run-test-runner.sh vilanoo-opencart 192.168.56.101 virgin-state /home/user/csrf/selenese-testcases/abantecart/Abantecart_12_user_logs_in.html 10 /home/user/csrf/firefox/firefox /home/user/.vilanoo/css-opencart-csrf-protected.db 0
```


# Analyze the CSRF Test results

Now that we have the test result databases we can start analyzing the test results to prepare for hit analysis in the `{deemon_folder}/rawtrace-analysis/src/`  by running 


```
./run-analyzer-2.sh -m <mosgi-csrf-db> -d <analyzed-csrf-db> -S <DBSchema> -f 0 -e 0
```


## Example

```
./run-analyzer-2.sh -m /home/user/.vilanoo/csrf-201708141444-mosgi-0.db -d /home/user/.vilanoo/csrf-201708141444-mosgi-0-analyzed.db -S ../../data/DBSchema.sql -f 0 -e 0
```

# Check for Hit

Finally we reached the stage where we can check whether or not we detected an csrf vulnerability.
