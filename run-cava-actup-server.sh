#!/bin/bash

# Should be run when cd'ed to the directory holding cava.actup-server.lisp

if [ $USER != "dfm" ] ; then
    SBCL=$HOME/lisp/bin/sbcl
    QUICKLISP=$HOME/lisp/quicklisp
else
    # kludge for testing and debugging on dfm's local machine
    SBCL=/usr/local/bin/sbcl
    QUICKLISP=$HOME/quicklisp
fi

if [ "$1" != "" ] ; then
    MODEL=$1
else
    MODEL="recency-model"
fi

$SBCL --no-userinit --load $QUICKLISP/setup --load cava-actup-server \
      --eval "(cava:run (quote cava:${MODEL}))"
