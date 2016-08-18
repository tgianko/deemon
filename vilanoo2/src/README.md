# vilanoo2

Vilanoo2 replaces Vilanoo. Vilanoo2 is based on proxy2 which can be found in proxy2/. As it extends proxy2, the documentation below is derived from the one of proxy2.


## Usage

Just run as a script:

```
$ python vilanoo2.py
```

Above command runs the proxy on tcp/8080.
To use another port, specify the port number as the first argument.

```
$ python vilanoo2.py 3128
```


## Enable HTTPS intercept

To intercept HTTPS connections, generate private keys and a private CA certificate:

```
$ ./setup_https_intercept.sh
```

Through the proxy, you can access http://proxy2.test/ and install the CA certificate in the browsers.
