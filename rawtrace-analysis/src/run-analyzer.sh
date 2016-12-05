#!/bin/bash

pwd=`pwd`

sbcl --dynamic-space-size 26000 --noinform --non-interactive --load "${pwd}/run-analyzer.lisp" "$@"
