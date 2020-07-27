#!/bin/sh

# Prep for build on Netlify.

# Create directory for any binaries we fetch
mkdir bin
export PATH="$(pwd)/bin:$PATH"

which elm
if [ $? != 0 ] ; then
    curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
    gunzip elm.gz
    chmod +x elm
    mv elm bin/elm
fi

which uglifyjs
if [ $? != 0 ] ; then
    npm install -g uglify-js@3.8
fi

which terser
if [ $? != 0 ] ; then
    npm install -g terser@4.8
fi
