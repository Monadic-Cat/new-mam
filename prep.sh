#!/bin/sh

# Prep for build on Netlify.

which elm
if [ $? != 0 ] ; then
    curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
    gunzip elm.gz
    chmod +x elm
    mkdir bin
    mv elm bin/elm
    export PATH="$(pwd)/bin:$PATH"
fi
