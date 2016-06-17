#!/bin/bash

sbcl --dynamic-space-size 4000 --noinform --non-interactive --load run-mosgi.lisp "$@"
