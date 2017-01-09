#!/usr/bin/env bash
if [ "$SELENESE_BASEURL" == "" ]; then
    echo "\$SELENESE_BASEURL needs to be set!"
    exit 1
fi

if [ "$SELENESE_FIREFOX" == "" ]; then
    SELENESE_FIREFOX=$(which firefox)
fi

java -jar selenese-runner.jar -i --driver firefox --firefox "$SELENESE_FIREFOX" --baseurl $SELENESE_BASEURL "$@"
