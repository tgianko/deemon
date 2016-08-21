# User Traces

To capture user traces we use Selenium IDE + Selenium IDE plugin for FireFox.

# Install:

Follow installation guide of Selenium IDE + Selenium IDE plugin.

# Generate trace files:
2. Open FireFox and run the Selenium IDE
3. Start a VM snapshop, and record a trace (it is very intuitive to do that)
4. From the Selenium IDE, save (or export.. I don't remember the exact action) 
the file in HTML format. This format is specific for Selenium and contains actions written in Selenese

A trace will look like that:


```html
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head profile="http://selenium-ide.openqa.org/profiles/test-case">
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
<link rel="selenium.base" href="http://192.168.56.101/" />
<title>opencart_new_user</title>
</head>
<body>
<table cellpadding="1" cellspacing="1" border="1">
<thead>
<tr><td rowspan="1" colspan="3">opencart_new_user</td></tr>
</thead><tbody>
<tr>
	<td>open</td>
	<td>/</td>
	<td></td>
</tr>
<tr>
	<td>click</td>
	<td>//div[@id='top-links']/ul/li[2]/a/span</td>
	<td></td>
</tr>
<tr>
	<td>clickAndWait</td>
	<td>link=Register</td>
	<td></td>
</tr>
<tr>
	<td>type</td>
	<td>id=input-firstname</td>
	<td>Giancarlo</td>
</tr>
<tr>
	<td>type</td>
	<td>id=input-lastname</td>
	<td>Pellegrino</td>
</tr>
```

# Programmatically reproduce traces

To reproduce early captured traces, we can use a modified version of 
[selenese-runner](https://github.com/tgianko/selenese-runner-java/tree/newfeat/interactive). 
The jar file is in this repository [here](https://projects.cispa.uni-saarland.de/giancarlo.pellegrino/vilanoo/blob/master/selenese-runner/selenese-runner-java-2.9.1-SNAPSHOT.jar).

It is a CLI that allows to chose driver, test case, speed, proxy and many other 
options. See the doc for more info. It worked out of the box on Ubuntu 16.04 LTS.

To run a test without the interactive mode, do the following:

```
$ java -cp selenese-runner-java-2.9.1-SNAPSHOT.jar jp.vmi.selenium.selenese.Main --driver firefox opencart_new_user.html --baseurl "http://192.168.56.102/"
```

where `opencart_new_user.html` is the trace captured with Selenium IDE and 
`http://192.168.56.102/` is the base URL of the Web Application under test.

To run a test in interactive mode, add `-i` ir `--interactive`:

```
$ java -cp selenese-runner-java-2.9.1-SNAPSHOT.jar jp.vmi.selenium.selenese.Main --driver firefox opencart_new_user.html --baseurl "http://192.168.56.102/" -i
```

# User trace capture and troubleshooting

## Create a Firefox profile

For performances reasons, we whitelist all `*.com`, `*.net`, and `*.org` domains. 
Resources from these tdomains are not requested throught the proxy. We use Firefox
as it supports this feature.

Selenese-runner allows to specify the proxy via command line. However, it does
not allow to specify whitelisted domains. For this reason, it is better to specify
proxy parameters via profile.

This is the part of `prefs.js` configuration that we use:

```javascript
user_pref("network.proxy.ftp", "127.0.0.1");
user_pref("network.proxy.ftp_port", 8080);
user_pref("network.proxy.http", "127.0.0.1");
user_pref("network.proxy.http_port", 8080);
user_pref("network.proxy.no_proxies_on", "*.com, *.net, *.org");
user_pref("network.proxy.share_proxy_settings", true);
user_pref("network.proxy.socks", "127.0.0.1");
user_pref("network.proxy.socks_port", 8080);
user_pref("network.proxy.ssl", "127.0.0.1");
user_pref("network.proxy.ssl_port", 8080);
user_pref("network.proxy.type", 1);
```

## Mouse Over events

Selenium IDE does not capture onMouseOver events. Selenese can perform these 
actions so consider to adjust selenese test cases manually to perform these 
operations.

## Headless Browser

Install xvfb and run it:
```
you$ sudo Xvfb :10 -ac
[sudo] password for you:
```

Set `DISPLAY` environment in the console you use for Firefx:
```
you$ export DISPLAY=:10 
```