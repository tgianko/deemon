#!/bin/bash

pwd=`pwd`

sbcl --dynamic-space-size 5000 --noinform --non-interactive --load "${pwd}/run-mosgi.lisp" "$@"


