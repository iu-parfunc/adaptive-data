#!/bin/bash

# This script responds to env vars:
#  * BENCHVARIANT - pure, scalable, hybrid

echo "Begin benchmarks for lockfree-test, running on $HOSTNAME"
set -xe

export HSBENCHER_GOOGLE_CLIENTID=759282369766-ijonhc4662ot2qos4lgud0e0sltjshlj.apps.googleusercontent.com
export HSBENCHER_GOOGLE_CLIENTSECRET=yI8GfZXsHPrW44udqklCHeDH

EXTRAARGS=$*

which -a stack
stack --version

# RESOLVER=lts-3.19
STACK="stack" # --install-ghc --resolver=$RESOLVER"
TARGET=adaptive-hashmap:bench-adaptive-hashmap
GRATIO=$2
IRATIO=$3

export GIT_DEPTH=`git log --pretty=oneline | wc -l`
echo "Running at GIT_DEPTH:" $GIT_DEPTH

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
#RTSOPTS=" -qa -qm -T -G1 "
RTSOPTS=" -qa "

REPORT=report_${executable}
BAKDIR=$HOME/benchdata_bak/$TABLENAME/depth_${GIT_DEPTH}/$executable
WINDIR=$BAKDIR/uploaded
FAILDIR=$BAKDIR/failed_upload

#mkdir -p $WINDIR
#mkdir -p $FAILDIR

# RRN: Temp, this should be factored out somewhere else:
# if ! [ -d $HOME/.stack ]; then
#     if [ -L $HOME/.stack ];
#     then echo "Warning: broken .stack link found in home dir.";
#          if [ -d /home.local/$USER ];
#          then mkdir -p /home.local/$USER/.stack;
#               ln -s -f /home.local/$USER/.stack $HOME/.stack;
#               echo "Made and linked .stack directory in non-NFS storage."
#          else echo
#          fi
#     else echo "No .stack dir but we can just let stack create it...";
#     fi
# fi


CRITUPLOAD="stack exec hsbencher-fusion-upload-criterion -- "
CSVUPLOAD="stack exec hsbencher-fusion-upload-csv -- "

# A first job for stack:
#time $STACK build hsbencher-fusion

CONFOPTS="--enable-benchmarks --allow-newer"

function runcritbench ()
{
    i=$1
    shift
#   benches=$*

    NURSERY_SIZE=$((30000/$i))
    echo "Using nursery of size $NURSERY_SIZE"

    CRITREPORT=${TAG}_${REPORT}${GRATIO}-{$IRATIO}-N$i.csv
    CSVREPORT=${TAG}_${REPORT}${GRATIO}-{$IRATIO}-N$i.csv

    $STACK bench $TARGET --benchmark-arguments="--bench=$BENCHVARIANT \
      --file=${TAG}_G${GRATIO}-I${IRATIO} --seed=$TAG --runs=25 -d=500 --gratio=$GRATIO --iratio=$IRATIO\
       +RTS -N$i $RTSOPTS "
# TODO: Pass EXTRAARGS

    # FIXME: does criterion uploader reorder for the server?
    # If not, our archived file below will not match the server schema.
    # time $CRITUPLOAD --noupload --csv=$CSVREPORT --variant=$VARIANT \
    # 		--threads=$i $CRITREPORT --runflags="$RTSOPTS" \
    #             --custom=HOT_RATIO,$HOT_RATIO --custom=CAS_TRIES,$CAS_TRIES,$benches
    # --args=""

    # NOTE: could aggregate these to ONE big CSV and then do the upload.

    # $CSVUPLOAD $CSVREPORT.2 --fusion-upload --name=$TABLENAME || FAILED=1
    # if [ "$FAILED" == 1 ]; then
    # 	cp $CSVREPORT $FAILDIR/
    # else
    # 	cp $CSVREPORT $WINDIR/
    # fi
}

function go() {


    REPORT=report_par_${executable}
    for i in $(seq $1); do
	    for BENCHVARIANT in nop pure cpure ctrie adaptive; do
		    runcritbench $i $BENCHVARIANT
	    done
    done

    # if [ $? = 0 ]; then
    # 	mkdir -p reports
    # 	cp ${TAG}_*.html reports
    # 	cp ${TAG}_*.crit reports
    # 	tar -cvf reports_${TAG}.tar reports/
    # else
    # 	echo "Some benchmarks errored out! Don't expect good data."
    # fi
}

go $1

mkdir "${TAG}_G${GRATIO}-I${IRATIO}"
mv ${TAG}_G${GRATIO}-I${IRATIO}_* ${TAG}_G${GRATIO}-I${IRATIO}/
cd ${TAG}_G${GRATIO}-I${IRATIO}/
gnuplot -e "filename='${TAG}_G${GRATIO}-I${IRATIO}' ; ncore=$1" ../bench.plt


