# INTRO

In RUN_TEST.md we gave a walkthrough on how to use the toolchain provided by deemon from installing the VM to using an oracle to decide whether 
a vulnerability was detected or not. However, we did not discuss the individual functionality of each tool. We want to remedy this
in this document by linking to the individual `README` of the tools. 

*NOTE: The documentations do not necessarily explain how to combine the tools successfully to check for csrf vulnerabilities. It took
quite some time to finetune and account for idiosyncracies we did not anticipate when we implemented or encountered them. Consequently, please refer
to our [simple test guide](./RUN_TEST.md) if you simply want to use our toolchain for csrf testing. It is also by no means meant to be an exhaustive explanation of 
potential use cases but rather a reference guide on what each tool is meant to accomplish and an explanation on how it does that in broad
strokes.*


# ZUMKA

[README](./zumka/README.md)

# VILANOO

[README](./vilanoo/src/README.md)

# MOSGI

[README](./mosgi/README.md)

# ANALYZER

[README](./rawtrace-analysis/README.md)

# DBMANAGER

[README](./deep-modeling/README.md#dbmanager)

# TESTMANAGER

[README](./deep-modeling/README.md#Testermanager)

# CSRF-TESTS-RUNNER

[README](./csrf-test-runner/README.md)
