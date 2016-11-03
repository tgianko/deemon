#!/usr/bin/env bash
java -jar selenese-runner.jar -i --driver firefox "$@" --baseurl $SELENESE_BASEURL
