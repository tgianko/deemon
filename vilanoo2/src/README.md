# vilanoo2

Vilanoo2 replaces Vilanoo. Vilanoo2 is based on proxy2 which can be found in `proxy2/`. As it extends proxy2, the documentation below is derived from the one of proxy2.

## Usage

A list of updated parameter is mantained here:

```terminal
$ ./vilanoo2.py --help
usage: vilanoo2.py [-h] [-b IP] [-p PORT] [-M IP] [-P PORT] -s PATH
                   [--disable-mosgi]

Main vilanoo2 proxy parameters

optional arguments:
  -h, --help            show this help message and exit
  -b IP, --bind IP      bind address
  -p PORT, --port PORT  listenig TCP port
  -M IP, --mosgi-address IP
                        MOSGI address
  -P PORT, --mosgi-port PORT
                        MOSGI port
  -s PATH, --sqlitedb PATH
                        SQLite3 DB
  --no-mosgi            Disable MOSGI
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
