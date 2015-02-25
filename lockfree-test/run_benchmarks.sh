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

# Run only certain benchmarks from the criterion suite:
# SEQBENCHES=""
PARBENCHES=" bag_team-parfill-N bag_new-1 bag_random5050-1000000 array-bag_hotcold-insert-100000 "

# Criterion regressions
REGRESSES="--regress=allocated:iters --regress=bytesCopied:iters --regress=cycles:iters \
--regress=numGcs:iters --regress=mutatorWallSeconds:iters --regress=gcWallSeconds:iters \
--regress=cpuTime:iters "

executable=bench-lockfree-test

TAG=`date +'%s'`

TABLENAME=AdaptivelyScalable

# These are good settings for insert benchmarks on cutter:
RTSOPTS=" -qa -qm -A1G -T -G1 "

REPORT=report_${executable}
BAKDIR=$HOME/benchdata_bak/$TABLENAME/depth_${GIT_DEPTH}/$executable
WINDIR=$BAKDIR/uploaded
FAILDIR=$BAKDIR/failed_upload

mkdir -p $WINDIR
mkdir -p $FAILDIR



CRITUPLOAD=hsbencher-fusion-upload-criterion-0.3.15
CSVUPLOAD=hsbencher-fusion-upload-csv-0.3.12
# If we don't have the Criterion uploader, don't bother trying
which $CRITUPLOAD

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

function runcritbench () {
    i=$1
    shift
    benches=$*

    CRITREPORT=${TAG}_${REPORT}-N$i.crit
    CSVREPORT=${TAG}_${REPORT}-N$i.csv

    ./dist/build/$executable/$executable $BENCHVARIANT $benches \
      --output=$CRITREPORT.html --raw $CRITREPORT $REGRESSES \
       +RTS -T -s -N$i $RTSOPTS -RTS

    # FIXME: does criterion uploader reorder for the server?
    # If not, our archived file below will not match the server schema.
    $CRITUPLOAD --noupload --csv=$CSVREPORT --variant=$VARIANT \
		--threads=$i $CRITREPORT --runflags="$RTSOPTS" \
                --custom=HOT_RATIO,$HOT_RATIO --custom=CAS_TRIES,$CAS_TRIES
    # --args=""

    # NOTE: could aggregate these to ONE big CSV and then do the upload.
    $CSVUPLOAD $CSVREPORT --fusion-upload --name=$TABLENAME || FAILED=1
    if [ "$FAILED" == 1 ]; then
	cp $CSVREPORT $FAILDIR/
    else
	cp $CSVREPORT $WINDIR/
    fi
    
}

function go() {    
    VARIANT=${BENCHVARIANT}-${BENCHCOMPILER}

    echo "Installing benchmark program."
    # Put the sandbox here in the lockfree-test/ subdir.
    $CABAL sandbox init
    # Grab the Chase-Lev deque packages from the submodule.
#    $CABAL install ../haskell-lockfree/chaselev-deque
    $CABAL install   $CONFOPTS -j --ghc-option=-j3 $EXTRAARGS . ../haskell-lockfree/chaselev-deque
    $CABAL configure $CONFOPTS -f-debug
    $CABAL build ${executable}
    
    # Remove old versions to prevent inconsistent data
    # for i in 1 2 4 8 16 32; do
    # 	CRITREPORT=$REPORT-N$i.crit
    # 	rm -f $CRITREPORT
    # 	rm -f $CRITREPORT.html
    # done

    echo "Listing supported benchmarks:"
    ./dist/build/$executable/$executable -l

    # REPORT=report_seq_${executable}
    # runcritbench 1 $SEQBENCHES

    REPORT=report_par_${executable}
    for i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 24 32; do
	runcritbench $i $PARBENCHES
    done

    if [ $? = 0 ]; then
	mkdir -p reports
	cp ${TAG}_*.html reports
	cp ${TAG}_*.crit reports
	tar -cvf reports_${TAG}.tar reports/
    else
	echo "Some benchmarks errored out! Don't expect good data."
    fi
}


case $BENCHVARIANT in
    pure)
	echo "Running pure-in-a-box benchmarks..."
	go
	;;
    oldpure)
	echo "Running old-style pure-in-a-box benchmarks..."
	go 
	;;
    scalable)
	echo "Running regular scalable/lock-free benchmarks..."
        go
	;;
    scalable-chaselev)
	echo "Running regular scalable/lock-free benchmarks..."	
	go 
	;;
    hybrid)
	echo "Running hybrid benchmarks..."
        for hot_ratio in 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0; do
	    HOT_RATIO=$hot_ratio go
        done
	;;
    *)
	echo "ERROR: unrecognized BENCHVARIANT: $BENCHVARIANT"
	exit 1
	;;
esac
