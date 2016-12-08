#!/bin/bash

pwd=`pwd`

sbcl --dynamic-space-size 10000 --noinform --non-interactive --load "${pwd}/run-analyzer.lisp" "$@"


while getopts 'm:v:S:d:' flag; do
    case ${flag} in
        d) ./sanity-check.sh ${OPTARG} ;;        
    esac
done
