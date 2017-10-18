# Introduction

This is a quick tutorial on how to install and use the toolchain Selenium IDE, 
Selenium IDE plugin, and our custom selenese-runner.

# Installation

## Requirements

User actions are recorded and replayed using Firefox. 

## Getting started

Download and install:

* Selenese IDE: Installation guide is here [http://www.seleniumhq.org/docs/02_selenium_ide.jsp](http://www.seleniumhq.org/docs/02_selenium_ide.jsp)
* Selenese IDE plugin for Firefox: You can find it here [https://addons.mozilla.org/en-US/firefox/addon/selenium-ide/](https://addons.mozilla.org/en-US/firefox/addon/selenium-ide/)

The other required component is `selenese-runner`. The original project is on GitHub,
but we modified with a new feature to run test cases/suites generated with Selenese IDE
in an interactive mode (See vilanoo) which is however now part of the main repository as well.
We compiled the shipped binary ourselfes but compiling the source code provided by the
[repository](https://github.com/vmi/selenese-runner-java) should lead to the same binary.

# Licence

As we did not write selenese-runner ourselves our licence does not apply but theirs which
can be found [here](https://github.com/vmi/selenese-runner-java/blob/master/LICENSE).

# Generate trace files

Selenese IDE can generate test cases and test suites reproducing user actions. To
do that, 

1. Lunch FireFox and run the Selenium IDE (you can run it as popup or as independend window)
2. Lunch the Web application under test
3. Interact with the HTML UI (it is very intuitive to do that, I am not going to give much more detail about that)
4. Save test cases and test suites in HTML.

## Test cases

A test case is an HTML file containing a sequence of Selenese commands. A complete list of selenese commands
can be found in the official documentation of Selenium ([http://docs.seleniumhq.org/docs/index.jsp](http://docs.seleniumhq.org/docs/index.jsp))

A test case will look like that:

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

## Test suite

A selenese test suite combines together 2 or more test cases. The format looks like this example"

```html
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
  <meta content="text/html; charset=UTF-8" http-equiv="content-type" />
  <title>Test Suite</title>
</head>
<body>
<table id="suiteTable" cellpadding="1" cellspacing="1" border="1" class="selenium"><tbody>
<tr><td><b>Test Suite</b></td></tr>
<tr><td><a href="abantecartTC01_registration.html">abantecartTC01_registration</a></td></tr>
<tr><td><a href="abantecartTC02_change_email.html">abantecartTC02_change_email</a></td></tr>
</tbody></table>
</body>
</html>
```

where `abantecartTC01_registration.html` and `abantecartTC02_change_email.html` are two
test cases.



# Selenese 

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

# Troubleshooting:

## Important notes on Selenese commands:

### Selenese test case does not work

*Cause 1*: Selenium IDE does not support all selenese commands. For example, events such as `mouseOver` are not supported. 

*Cause 2*: Selenese runner does not support all selenese commands.

*Solution*: Find a different path in the web application. In case of Cause 1, if the command is supported by selenese runner, then consider to add the missing command manually. In case of Cause 2, use a different command.

### I want to run Firefox in headless mode

Install xvfb and run it:
```
you$ sudo Xvfb :10 -ac
[sudo] password for you:
```

Set `DISPLAY` environment in the console you use for Firefx:
```
you$ export DISPLAY=:10 
```

In this case, for debugging reasons, you may want to consider to capture screenshots at each step. Use `-S` with `selenese-runner`

### Selenese runner crashes

*Cause 1*: You are using Firefox 47.0. There is known bug between the selenium driver used by selenese-runner and Firefox 47.0 (See [here](https://github.com/SeleniumHQ/selenium/issues/2204))

*Solution*: Install Firefox 48.0. If you install it separately, then user `--firefox <path>` pass the executable path 


