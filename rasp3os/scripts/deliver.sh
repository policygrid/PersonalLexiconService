#!/bin/sh
#
# Deliver.sh - make image for sbcl

HERE=${RASP:-/home/`whoami`/rasp3os}/scripts

arch=`uname -m | sed "s/i.86/ix86/"`_`uname -s | tr "[:upper:]" "[:lower:]"`
if [ $arch = ix86_darwin ] && [ `sysctl -n hw.optional.x86_64` = 1 ]; 
then
    arch=x86_64_darwin;
fi

export SBCL_HOME=/usr/local/lib/sbcl
export LISP=/usr/local/bin/sbcl

rm -rf $HERE/../gde/$arch/gde
mkdir -p $HERE/../gde/$arch

rm -rf $HERE/../gde/cgde/*.fasl $HERE/../gde/cpar/*.fasl $HERE/../prob/code/*.fasl $HERE/../mrs/rmrs/*.fasl $HERE/../mrs/xml/*.fasl

# Load lisp sources

$LISP <<EOF

(load "$HERE/../gde/cgde/load.lsp")
(load "$HERE/../gde/cpar/load.lsp")
(load "$HERE/../prob/code/load.lsp")

(save-gde-image "$HERE/../gde/$arch/gde" :executable t)

(quit)
EOF
