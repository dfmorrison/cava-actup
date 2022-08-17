#!/bin/bash

# Should be run when cd'ed to the directory holding cava.actup-server.lisp

# SBCL=/usr/local/bin/sbcl
SBCL=$HOME/lisp/bin/sbcl

# QUICKLISP=$HOME/quicklisp
QUICKLISP=$HOME/lisp/quicklisp

$SBCL --no-userinit --load $QUICKLISP/setup --load cava-actup-server \
      --eval "(cava:run (quote cava:recency-model))"
