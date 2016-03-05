#!/bin/bash

set -xe

which -a stack
stack --version

TOP=`pwd`

# Building our current/recent version [2016.03.05]:
cd "$TOP/adaptive-hashmap/"
stack --install-ghc --no-system-ghc build

# Building the top-level repo builds a bunch of the old code:
cd "$TOP/"
stack --install-ghc --no-system-ghc build
