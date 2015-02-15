#!/bin/bash

echo "FINISHME: invoke java benchmarks here and then upload"
set -xe

case $BENCHVARIANT in
    pure)
	echo "FINISHME: Run pure-in-a-box benchmarks here..."
	;;
    scalable)
	echo "FINISHME: run scalable benchmarks here"
	;;
    hybrid)
	echo "FINISHME: run hybrid benchmarks here"
	;;
    *)
	echo "ERROR: unrecognized BENCHVARIANT: $BENCHVARIANT"
	exit 1
	;;
esac

which -a hsbencher-fusion-upload-csv 
# This can upload the CSV file:
hsbencher-fusion-upload-csv -h
