# Trace Acquisition

This is a quick tutorial on the acquisition of traces from a VM. This tutorial
covers the following toolchains:

 * **CASE 1**: Browser + vilanoo2 + mosgi + rawtrace-analysis
 * **CASE 2**: Selenese test case/suite + vilanoo2 + mosgi + rawtrace-analysis

## CASE 1: Browser + vilanoo2 + mosgi + rawtrace-analysis

This covers use cases in which you would like to use your own browser to visit
the web application and capture dynamic traces from the network and the VM.

At this point we assume you prepared VM images with [zumka](zumka/README.md). 
If not, have a look at the documentation on how to use zumka. We also assume
that you installed everything correctly.

In general, the steps that you will need to follow are:

 1. Run mosgi and vilanoo2;
 2. Configure your browser to use vilanoo2 as proxy and play around;
 3. Alt mosgi and vilanoo2;
 4. Run rawtrace-analysis;

### Run mosgi and vilanoo2

The first step is to start mosgi: 

```
cd mosgi/src
./run.sh -h
```

It is likely that you will use the following command line:

```
./vilanoo/mosgi/run.sh -x /tmp/ -P /opt/bitnami/php/tmp/ -p 9292 -i 127.0.0.1 -t 192.168.56.101 -r root -c bitnami -s $path_to_your_mosgi_sqlitedb
```

After that, Mosgi is up and running waiting for incoming connections at 
localhost port 9292. 

Now, run vilanoo2 (to intercept also HTTPS request, please read 
[this](vilanoo2/src/README.md):

```bash
cd vilanoo2/
./vilanoo2.py -s $path_to_your_vilanoo_sqlitedb
```

### Change proxy configuration of your browser and play around

Come on. If you do need help on this, please consider to leave this page and
find another job ;)

### Analysis of raw traces.

Vilanoo2 and Mosgi generate **raw** traces. The output of these two tools are 
two SQLite3 databases. Starting from these databases, dyntrace extract traces 
with SQL operations, session data snapshots, and disk operations.

```bash
cd rawtrace-analysis/src/
./run-analyzer.sh -m $path_to_your_mosgi_sqlitedb -v $path_to_your_vilanoo_sqlitedb -d $path_to_your_rawtraceanalysis_sqlitedb -S ../../data/DBSchema.sql
```

This will create a new SQLiteDB3 `$path_to_your_rawtraceanalysis_sqlitedb` from 
the analysis on `$path_to_your_mosgi_sqlitedb` and `$path_to_your_vilanoo_sqlitedb`.


## CASE 2: Selenese test case/suite + vilanoo2 + mosgi + rawtrace-analysis

This covers use cases in which you have prepared a Selenese test case or test 
suite (in HTML format!). Make sure you have tested the Selenese HTML file and
that it works properly.

We assume you prepared VM images with [zumka](zumka/README.md). If not, have a 
look at the documentation on how to use zumka. We also assume that you installed 
everything correctly. Furthermore, we assume also that you know how to create
Selenese HTML files and you are capable of verifying that they work smoothly.

In general, the steps of this use case are:

 1. Run mosgi and vilanoo2 with a Selenese HTML file as input;
 2. Run rawtrace-analysis;

### Run mosgi and vilanoo2 with a Selenese HTML file as input

To run mosgi, it is likely that you will use the following command line:

```
cd mosgi/src
./vilanoo/mosgi/run.sh -x /tmp/ -P /opt/bitnami/php/tmp/ -p 9292 -i 127.0.0.1 -t 192.168.56.101 -r root -c bitnami -s $path_to_your_mosgi_sqlitedb
```

After that, Mosgi is up and running waiting for incoming connections at 
localhost port 9292. 

Now, run vilanoo2 and pass as parameter the filename of the Selenese HTML file:

```bash
cd vilanoo2/
./vilanoo2.py -s $path_to_your_vilanoo_sqlitedb -S $path_to_your_selenese_file
```

The `-S` or `--selenese` parameter will instruct vilanoo2 to run selenese-runner
in interactive mode. Selenese-runner will be configured to use vilanoo2 as a proxy.

When vilanoo2 is used with `-S`, then vilanoo2 will store in the
SQLite database `$path_to_your_vilanoo_sqlitedb` the list of selenese commands 
and correlate them with the HTTP requests. 


contain also 
