#!/bin/bash

# This script responds to env vars:
#  * BENCHVARIANT - pure, scalable, hybrid

echo "Begin benchmarks for lockfree-test."
set -xe

export HSBENCHER_GOOGLE_CLIENTID=759282369766-ijonhc4662ot2qos4lgud0e0sltjshlj.apps.googleusercontent.com
export HSBENCHER_GOOGLE_CLIENTSECRET=yI8GfZXsHPrW44udqklCHeDH

EXTRAARGS=$*

which -a stack
stack --version

RESOLVER=lts-3.5
STACK="stack --install-ghc --resolver=$RESOLVER"
TARGET=adaptive-bag:bench-adaptive-bag

# PARBENCHES=" array-bag_hotcold-team-fill-insert-10000000 array-bag_hotcold-team-fill-N "
PARBENCHES=""

# Criterion regressions
REGRESSES="--regress=allocated:iters --regress=bytesCopied:iters --regress=cycles:iters \
--regress=numGcs:iters --regress=mutatorWallSeconds:iters --regress=gcWallSeconds:iters \
--regress=cpuTime:iters "

executable=bench-lockfree-test

TAG=`date +'%s'`

TABLENAME=AdaptivelyScalable

# These are good settings for insert benchmarks on cutter:
RTSOPTS=" -qa -qm -T -G1 "

REPORT=report_${executable}
BAKDIR=$HOME/benchdata_bak/$TABLENAME/depth_${GIT_DEPTH}/$executable
WINDIR=$BAKDIR/uploaded
FAILDIR=$BAKDIR/failed_upload

mkdir -p $WINDIR
mkdir -p $FAILDIR

CRITUPLOAD="stack exec hsbencher-fusion-upload-criterion -- "
CSVUPLOAD="stack exec hsbencher-fusion-upload-csv -- "

CONFOPTS="--enable-benchmarks --allow-newer"

function runcritbench ()
{
    i=$1
    shift
    benches=$*

    NURSERY_SIZE=$((30000/$i))
    echo "Using nursery of size $NURSERY_SIZE"

    CRITREPORT=${TAG}_${REPORT}${HOT_RATIO}-N$i.crit
    CSVREPORT=${TAG}_${REPORT}${HOT_RATIO}-N$i.csv

    $STACK bench $TARGET --benchmark-arguments="$BENCHVARIANT $benches \
      --output=$CRITREPORT.html --raw $CRITREPORT $REGRESSES \
       +RTS -T -s -N$i $RTSOPTS -A${NURSERY_SIZE}M"

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
    VARIANT=${BENCHVARIANT}-${RESOLVER}

    echo "Listing supported benchmarks:"
    $STACK bench $TARGET --benchmark-arguments="-l"

    REPORT=report_par_${executable}
    for i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16; do
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
    scalable)
	echo "Running regular scalable/lock-free benchmarks..."
        go
	;;
    hybrid)
	echo "Running hybrid benchmarks..."
        for cas_tries in 1 2; do
	    CAS_TRIES=$cas_tries go
        done
	;;
    *)
	echo "ERROR: unrecognized BENCHVARIANT: $BENCHVARIANT"
	exit 1
	;;
esac
