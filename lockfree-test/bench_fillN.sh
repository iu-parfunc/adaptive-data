#!/bin/bash

# Just a convenience script for running quick checks by hand.

# MODES="pure scalable"
MODES="hybrid scalable"
BENCHES="team-parfill-N"
# perthreadop-parfill-N 

# MODES=" basic "
# BENCHES=" team-separate-bag "

# RTSOPTS="-qa -T -s -A20M "

# GHCVER=7.8.3
GHCVER=7.10.rc2

# These are the preferred GC settings for insert benchmarks:
RTSOPTS=" -qa -qm -A1G -T -G1 "
# -c doesn't hurt much, but it doesn't really help.
# and it isn't supported by -G1.
#  Likewise -qb0 and -qg0

set -xe

CMD=./dist/build/bench-lockfree-test/bench-lockfree-test
REGRESSES="--regress=allocated:iters --regress=bytesCopied:iters --regress=cycles:iters --regress=numGcs:iters --regress=mutatorWallSeconds:iters --regress=gcWallSeconds:iters --regress=cpuTime:iters"

outdir="./html_reports/"
mkdir -p $outdir

CONFOPTS="--with-ghc=ghc-$GHCVER --with-ghc-pkg=ghc-pkg-$GHCVER -f-debug --enable-benchmarks --allow-newer"

cabal clean
if ! [ -e $CMD ]; then
    cabal install --only-dependencies $CONFOPTS -j --ghc-option=-j3
    cabal configure $CONFOPTS
    cabal build bench-lockfree-test
    $CMD -l
fi

for bench in $BENCHES; do 
  for mode in $MODES; do
#   for threads in 1 2 3 4; do 		
   for threads in 1 2 4 6 8 12 16; do
       $CMD $mode/${bench} $REGRESSES -o $outdir/${mode}-${bench}${threads}.html +RTS -N${threads} $RTSOPTS
     done
   done
done

