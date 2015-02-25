#!/bin/bash

set -xe

TABLENAME=AdaptivelyScalable
CSVUPLOAD=hsbencher-fusion-upload-csv-0.3.9

MAVEN=mvn
JAVA_EXEC=java
JAVA_OPTS="-Xms16g -Xmx24g -d64"
JAVA_RUN="$JAVA_EXEC $JAVA_OPTS"

if [[ "$HOSTNAME" =~ cutter ]]; then
    module load jdk
fi

$MAVEN clean
$MAVEN package 

BENCHMARK_ROUNDS=100
MAX_NUM_THREADS=16
NUM_INSERTS=1000000

NUM_HOTKEYS=1
TIME_MILLI_SECONDS="$(date +%s)"

case $BENCHVARIANT in
    "oldpure")
	echo "This variant is only for Haskell, it does nothing on the Java side."
	;;    
    "pure")
	echo "Running pure-in-a-box benchmark"
	#for i in 1;
    #do
		#	$JAVA_RUN -cp  target/j_benchmark-0.0.1.jar benchmark.SimpleInsertionBenchmark $BENCHVARIANT $(($NUM_INSERTS * $i)) $BENCHMARK_ROUNDS $MAX_NUM_THREADS $TIME_MILLI_SECONDS 
	#done
	for HOT_RATIO in 0.0 0.1 0.2 0.3 0.4 0.5;
	do
		$JAVA_RUN -cp  target/j_benchmark-0.0.1.jar benchmark.RandomHotColdkeyBecnhmark $BENCHVARIANT $NUM_INSERTS $NUM_HOTKEYS $BENCHMARK_ROUNDS $MAX_NUM_THREADS $HOT_RATIO $CAS_TRIES $TIME_MILLI_SECONDS
	done  
	;;
    "scalable")
	echo "Running scalable benchmarks"
	for HOT_RATIO in 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0;
	do
		$JAVA_RUN -cp  target/j_benchmark-0.0.1.jar benchmark.RandomHotColdkeyBecnhmark $BENCHVARIANT $NUM_INSERTS $NUM_HOTKEYS $BENCHMARK_ROUNDS $MAX_NUM_THREADS $HOT_RATIO $CAS_TRIES $TIME_MILLI_SECONDS
	done 
	;;
    "hybrid")
	echo "Running hybrid benchmarks here"
	for HOT_RATIO in 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0;
	do
		$JAVA_RUN -cp  target/j_benchmark-0.0.1.jar benchmark.RandomHotColdkeyBecnhmark $BENCHVARIANT $NUM_INSERTS $NUM_HOTKEYS $BENCHMARK_ROUNDS $MAX_NUM_THREADS $HOT_RATIO $CAS_TRIES $TIME_MILLI_SECONDS
	done 
	;;
    *)
echo "ERROR: unrecognized BENCHVARIANT: $BENCHVARIANT"
	exit 1
	;;
esac


$CSVUPLOAD "$TIME_MILLI_SECONDS"_random_hot_cold_key.csv --fusion-upload --name=$TABLENAME
$CSVUPLOAD "$TIME_MILLI_SECONDS"_simple_insertion.csv --fusion-upload --name=$TABLENAME

#which -a hsbencher-fusion-upload-csv 
# This can upload the CSV file:
#hsbencher-fusion-upload-csv -h 
