#!/bin/bash

# Benchmark runner for the whole repository.  Dispatch to the right place:

# We expect whoever launched us to set these environment variables:
#  * BENCHVARIANT  = pure, scalable, or hybrid
#  * BENCHCOMPILER = a ghc executable name or "java"
#  * CI_BUILD_ID   = build identifier.

echo "Begin benchmarks for Adaptive data repository."
set -x
set -e

EXTRAARGS=$*

if [ "$MACHINECLASS" == "" ]; then
    export MACHINECLASS=`hostname -s`
fi

echo "On linux platforms, check CPU affinity:"
taskset -pc $$ || echo ok

echo "Also check load:"
sar 2 2 || echo ok

echo "And who"
who -a || echo ok

# Switch to the top of the repo:
cd `dirname $0`

# CONVENTION: The working directory is passed as the first argument.
CHECKOUT=$1
shift || echo ok
if [ "$CHECKOUT" == "" ]; then
    CHECKOUT=`pwd`
fi

if [ "$BENCHCOMPILER" == "" ]; then
    export BENCHCOMPILER=ghc-7.8.3
    echo "BENCHCOMPILER unset, defaulting to $BENCHCOMPILER"
fi

echo "Running benchmarks remotely on server `hostname`"

case $BENCHCOMPILER in
    java)
	echo "Running Java version of benchmarks"
	cd j_benchmark
	echo "FINISHME: put a script here."
	;;
    ghc-*)
	echo "Running GHC version of benchmarks."
	cd lockfree-test
	# ./run_benchmarks.sh
	echo "FINISHME: not running script yet"
	;;
    *)
	echo "ERROR: unrecognized BENCHCOMPILER: $BENCHCOMPILER"
	exit 1
	;;
esac
