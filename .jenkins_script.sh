#!/bin/bash

echo "Jenkins script, starting up.  Running on "`hostname`

set -xe

which -a stack
stack --version

TOP=`pwd`

# Building our current/recent version [2016.03.05]:
cd "$TOP/adaptive-hashmap/"
source setup_env.sh
which -a ghc
ghc --version
stack build

set +xe
echo "Finished with critical part of test."
echo "Now testing old code but not counting failures..."

# Building the top-level repo builds a bunch of the old code.
# This uses an LTS build so it should ignore the GHC on path.
cd "$TOP/"
stack --install-ghc --no-system-ghc build
