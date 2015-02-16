#!/bin/bash

echo "FINISHME: invoke java benchmarks here and then upload"
set -xe
SIMPLE_INSERTION_OUTPUT_FILE=simple_insertion.csv
RANDOM_HOT_COLD_OUTPUT_FILE=random_hot_cold_key.csv

JAVA_EXEC=java
JAVA_OPTS="-Xms4g -Xmx16g -d64"
JAVA_RUN="$JAVA_EXEC $JAVA_OPTS"


BENCHMARK_ROUNDS=100
MAX_NUMBER_OF_THREADS=64
NUMBER_OF_INSERTS=1000000

COLD_KEY_OPERATION_CHANCE=0.5
HOT_KEY_PERCENTAGE=1.0

#rm -rf $SIMPLE_INSERTION_OUTPUT_FILE
#rm -rf $RANDOM_HOT_COLD_OUTPUT_FILE

case $BENCHVARIANT in
    "oldpure")
	echo "This variant is only for Haskell, it does nothing on the Java side."
	;;    
    "pure")
	echo "Running pure-in-a-box benchmark"
	for i in 1 2 4 8 16;
    do
		$JAVA_RUN -cp  target/j_benchmark-0.0.1.jar benchmark.SimpleInsertionBenchmark $BENCHVARIANT $(($NUMBER_OF_INSERTS * $i)) $BENCHMARK_ROUNDS $MAX_NUMBER_OF_THREADS 
	done

	$JAVA_RUN -cp  target/j_benchmark-0.0.1.jar benchmark.RandomHotColdkeyBecnhmark $BENCHVARIANT $NUMBER_OF_INSERTS $HOT_KEY_PERCENTAGE $BENCHMARK_ROUNDS $MAX_NUMBER_OF_THREADS $COLD_KEY_OPERATION_CHANCE
	;;
    "scalable")
	echo "Running scalable benchmarks"
	for i in 1 2 4 8 16;
        do
		$JAVA_RUN -cp  target/j_benchmark-0.0.1.jar benchmark.SimpleInsertionBenchmark $BENCHVARIANT  $(($NUMBER_OF_INSERTS * $i)) $BENCHMARK_ROUNDS $MAX_NUMBER_OF_THREADS 
	done

	$JAVA_RUN -cp  target/j_benchmark-0.0.1.jar benchmark.RandomHotColdkeyBecnhmark $BENCHVARIANT $NUMBER_OF_INSERTS $HOT_KEY_PERCENTAGE $BENCHMARK_ROUNDS $MAX_NUMBER_OF_THREADS $COLD_KEY_OPERATION_CHANCE
	;;
    "hybrid")
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
