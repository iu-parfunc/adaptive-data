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

TAG=`date +'%s'`

REPORT=report_${executable}

TABLENAME=AdaptivelyScalable


CRITUPLOAD=hsbencher-fusion-upload-criterion-0.3.8
CSVUPLOAD=hsbencher-fusion-upload-csv-0.3.8
# If we don't have the Criterion uploader, don't bother trying
if ! [ -x `which $CRITUPLOAD` ]; then
    echo "Error: no $CRITUPLOAD found"
    exit 1
fi

# CABAL=cabal-1.20
# Cabal 1.20 has working parallel builds but it is too old to drive
# GHC 7.10 it seems.  We get this error:
#
#    ghc: ghc no longer supports single-file style package databases
#    (dist/package.conf.inplace) use 'ghc-pkg init' to create the
#    database with the correct format.
#
# E.g. in build #7.
CABAL=cabal-1.22

CONFOPTS="--enable-benchmarks --allow-newer"

function go() {    
    VARIANT=${BENCHVARIANT}-${BENCHCOMPILER}
    
    echo "Installing benchmark program."
    # Put the sandbox here in the lockfree-test/ subdir.
    $CABAL sandbox init
    $CABAL install   $CONFOPTS -j --ghc-option=-j3 $EXTRAARGS 
    $CABAL configure $CONFOPTS -f-debug
    $CABAL build ${executable}
    
    # Remove old versions to prevent inconsistent data
    # for i in 1 2 4 8 16 32; do
    # 	CRITREPORT=$REPORT-N$i.crit
    # 	rm -f $CRITREPORT
    # 	rm -f $CRITREPORT.html
    # done

    for i in 1 2 4 8 16 32; do
	CRITREPORT=${TAG}_${REPORT}-N$i.crit
	CSVREPORT=${TAG}_${REPORT}-N$i.csv
	
	./dist/build/$executable/$executable $BENCHVARIANT \
	    --output=$CRITREPORT.html --raw $CRITREPORT $REGRESSES +RTS -T -s -N$i

        $CRITUPLOAD --noupload --csv=$CSVREPORT --variant=$VARIANT --threads=$i $CRITREPORT
	# --args=""

	# NOTE: could aggregate these to ONE big CSV and then do the upload.
        $CSVUPLOAD $CSVREPORT --fusion-upload --name=$TABLENAME 
    done

    if [ $? = 0 ]; then
	mkdir -p reports
	cp ${TAG}_*.html reports
	cp ${TAG}_*.crit reports
	tar -cvf reports.tar reports/
    else
	echo "Some benchmarks errored out! Don't expect good data."
    fi
}

case $BENCHVARIANT in
    pure)
	echo "Running pure-in-a-box benchmarks..."
	go 
	;;
    scalable)
	echo "Running regular scalable/lock-free benchmarks..."	
	go 
	;;
    hybrid)
	echo "Running hybrid benchmarks..."
	go 
	;;
    *)
	echo "ERROR: unrecognized BENCHVARIANT: $BENCHVARIANT"
	exit 1
	;;
esac
