# vilanoo2

Vilanoo2 replaces Vilanoo. Vilanoo2 is based on proxy2 which can be found in `proxy2/`. As it extends proxy2, the documentation below is derived from the one of proxy2.

# Features:

Vilanoo2 can:

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

This is most likely the command line that you are going to use more often:

```
$ ./vilanoo2.py -s $path_to_your_sqlitedb
```

If you don't want to connect to MOSGI use `--no-mosgi`:

```
$./vilanoo2.py -s $path_to_your_sqlitedb --no-mosgi
```


## Enable HTTPS intercept

To intercept HTTPS connections, generate private keys and a private CA certificate:

```
$ ./setup_https_intercept.sh
```

Through the proxy, you can access http://proxy2.test/ and install the CA certificate in the browsers.
