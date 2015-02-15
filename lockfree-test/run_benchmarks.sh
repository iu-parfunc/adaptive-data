#!/bin/bash

echo "Begin benchmarks for lockfree-test."
set -xe

EXTRAARGS=$*

if [ "$BENCHCOMPILER" == "" ]; then
    export BENCHCOMPILER=ghc-7.8.3
    echo "BENCHCOMPILER unset, defaulting to $BENCHCOMPILER"
fi
export PATH=$HOME/opt/$BENCHCOMPILER/bin:$PATH
which $BENCHCOMPILER
which cabal
cabal --version
which ghc
ghc --version

# Criterion regressions
REGRESSES="--regress=allocated:iters --regress=bytesCopied:iters --regress=cycles:iters \
--regress=numGcs:iters --regress=mutatorWallSeconds:iters --regress=gcWallSeconds:iters \
--regress=cpuTime:iters "

executable=bench-lockfree-test

# Put the sandbox here in the lockfree-test/ subdir.
cabal sandbox init

echo "Installing benchmark program."

TAG=`date +'%s'`

cabal install   --enable-benchmarks $EXTRAARGS
cabal configure --enable-benchmarks -f-debug
cabal build ${executable}

REPORT=report_${executable}

function go() {
    WHICHBENCH=$*
    
    # Remove old versions to prevent inconsistent data
    for i in 1 2 4 8 16 32; do
	CRITREPORT=$REPORT-N$i.crit
	rm -f $CRITREPORT
	rm -f $CRITREPORT.html
    done

    for i in 1 2 4 8 16 32; do
	CRITREPORT=$REPORT-N$i.crit
	./dist/build/$executable/$executable \
	    --output=$CRITREPORT.html --raw $CRITREPORT $REGRESSES +RTS -T -s -N$i
    done

    if [ $? = 0 ]; then
	mkdir -p reports
	cp *.html reports
	cp *.crit reports
	tar -cvf reports.tar reports/
    else
	echo "Some benchmarks errored out! Don't expect good data."
    fi
}

case $BENCHVARIANT in
    pure)
	echo "Running pure-in-a-box benchmarks..."
	# FIXME: use standardized names:
	go new/PureBag random-50-50/PureBag hotkey/PureBag
	;;
    scalable)
	echo "FINISHME: run scalable benchmarks here"
	    # "new/ScalableBag" \
	    # "random-50-50/ScalableBag" \
	    # "hotkey/ScalableBag" \
	;;
    hybrid)
	echo "FINISHME: run hybrid benchmarks here"
	    # "new/AdaptiveBag" \
    	    # "random-50-50/AdaptiveBag" \
	    # "hotkey/AdaptiveBag" \
	;;
    *)
	echo "ERROR: unrecognized BENCHVARIANT: $BENCHVARIANT"
	exit 1
	;;
esac
