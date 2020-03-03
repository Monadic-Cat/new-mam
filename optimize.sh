#!/bin/sh

# Make sure we've got an Elm binary to work with.
. prep.sh

ELM_FLAGS=--optimize make

uglifyjs dist/main.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=dist/main.js
