#!/bin/sh

# Textual replacements in build output for things like environment variable configuration.

# Let's just use handlebar syntax.
sed -i "s+{{AUTH_ENDPOINT}}+$AUTH_ENDPOINT+g" dist/index.html
sed -i "s+{{SYNC_ENDPOINT}}+$SYNC_ENDPOINT+g" dist/index.html
