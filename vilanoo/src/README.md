# vilanoo

Vilanoo is based on proxy2 which can be found in `proxy2/`. As it extends proxy2, the documentation below is derived from the one of proxy2.

# Features

Vilanoo can:

* Capture HTTP request and responses (in the current version, scope and request filtering is hardcoded)
* Execute Selenese test cases and test suites (in combination with `selenese-runner` in interactive mode)
* Correlate selenese commands with HTTP requests
* Coordinate with `mosgi` to capture server-side traces 

# Usage

A list of updated parameter is mantained here:

```terminal
$ ./vilanoo2.py --help
usage: vilanoo2.py [-h] [-b IP] [-p PORT] [-M IP] [-P PORT] -s PATH
                   [--no-mosgi] [-S PATH] [--selenese-args PATH] [-w SEC]

Main vilanoo2 proxy parameters

optional arguments:
  -h, --help            show this help message and exit
  -b IP, --bind IP      Vilanoo proxy binding IPv4 address. This address is
                        also used for the proxy configuration of selenese-
                        runner.
  -p PORT, --port PORT  TCP port for the Vilanoo proxy. This port is also used
                        for the proxy configuration of selenese-runner.
  -M IP, --mosgi-address IP
                        MOSGI listening address.
  -P PORT, --mosgi-port PORT
                        MOSGI TCP port.
  -s PATH, --sqlitedb PATH
                        SQLite3 DB file.
  --no-mosgi            By default, MOSGI is enabled. Use this option to
                        disable MOSGI.
  -S PATH, --selenese PATH
                        Specify the selenese test case/suite to run. Vilanoo
                        uses selenese-runner-java (modified to be
                        interactive).
  --selenese-args PATH  Use this parameter to pass additional CLI arguments to
                        selenese-runner-jave
  -w SEC, --wait SEC    Waiting time in seconds before the next Selenese
                        command is executed.

```

## Capture HTTP traffic only

Vilanoo stores HTTP traffic into a SQLite3 database. By default coordination with `mosgi` is enabled and you need to turn it off.

You can do that as follows:

```terminal
$ ./vilanoo2.py -s sqlite_file.db --no-mosgi
```

where `sqlite_file.db` is the SQLite3 database. You can use a absolute or relative path. If the path does not exist, vilanoo creates it.

## Running Vilanoo with mosgi

Vilanoo sets up a TCP connection with mosgi. Accordingly, you will need to run mosgi before running vilanoo.
Then, type the following:

```
$./vilanoo2.py -s sqlite_file.db
```

Vilanoo will connect to the default port and IP where mosgi should be already listeining to. If this fails, vilanoo will shutdown. 

## Running Vilanoo with selenese-runner

Here, we show how to use vilanoo with selenese-runner. To simplify the example, we will not use mosgi.

The interface between vilanoo and selenese-runner is different then the one with mosgi. Vilanoo takes in input selenese test cases/suites
and passes them to selenese runner. The parameter `-S` specifies the path of the test case/suite filename:

```
./vilanoo2.py --no-mosgi -s sqlite_file.db -S ../../selenese-runner/testcases/abantecartTS01_change_email.html 
```

This will tell vilanoo to load the test suite, parse it and store it in the `sqlite_file.db`. Then, vilanoo will run `selenese-runner` in 
interactive mode. All HTTP requests/responses are stored in the DB and associated with the executed command. Then vilanoo will send a `\n` 
byte to selenese-runner in order to execute the next command.

When used with mosgi, the DB will also contains XDebug traces, session data, and files being accessed by the web application under test.


# Enable HTTPS intercept (from proxy2)

To intercept HTTPS connections, generate private keys and a private CA certificate:

```
$ ./setup_https_intercept.sh
```

Through the proxy, you can access http://proxy2.test/ and install the CA certificate in the browsers.
