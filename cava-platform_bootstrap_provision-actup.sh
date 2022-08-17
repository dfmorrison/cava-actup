#!/bin/bash
source /tmp/provision-check.sh 

# Installs SBCL and Quicklisp, and an assortment of Quicklisp packages.
# Should be run as the user who will be running SBCL.
# It will create a subdirectory, lisp/, in that user's home directory
# as well as scribbling into ~/.cache/common-lisp.
# Assumes curl and tar are available in the user's PATH.
# Does not bother checking any checksums of downloaded code.
# Change the value below of SBCL_NAME to download a different version, if preferred.

SBCL_NAME=sbcl-2.2.6-x86-64-linux
SBCL_ARCHIVE=${SBCL_NAME}-binary.tar.bz2
SBCL_URL=https://sourceforge.net/projects/sbcl/files/sbcl/2.2.6/${SBCL_ARCHIVE}/download
QUICKLISP_URL=https://beta.quicklisp.org/quicklisp.lisp

HOME="/home/vagrant/"

apt-get install -y curl

cd /tmp
curl -L $SBCL_URL -o $SBCL_ARCHIVE
curl -O $QUICKLISP_URL
tar -xf $SBCL_ARCHIVE
rm $SBCL_ARCHIVE
mkdir -p $HOME/lisp
pushd $SBCL_NAME
INSTALL_ROOT=$HOME/lisp bash install.sh
popd
rm -rf $SBCL_NAME
$HOME/lisp/bin/sbcl --no-userinit --load 'quicklisp.lisp' \
                --eval '(quicklisp-quickstart:install :path "~/lisp/quicklisp/")' \
                --eval "(ql:quickload '(:cl-interpol :alexandria :iterate :cl-ppcre :uiop))" \
                --eval "(ql:quickload '(:usocket-server :bordeaux-threads :local-time :uuid))" \
                --eval "(ql:quickload '(:vom :trivial-backtrace))" \
                --eval '(sb-ext:exit)'
rm quicklisp.lisp

chown -R vagrant:vagrant $HOME/lisp
