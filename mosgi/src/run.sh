#!/bin/bash

pwd=`pwd`

sbcl --dynamic-space-size 10000 --noinform --non-interactive --load "${pwd}/run-mosgi.lisp" "$@"


