#!/bin/bash
SEED=`date +'%s'`
stack bench adaptive-hashmap:pldi-bench --benchmark-arguments "--bench ctrie    --dir data --seed ${SEED} --ops 100000 --unit 1000 --ndb 10 +RTS -N16"
stack bench adaptive-hashmap:pldi-bench --benchmark-arguments "--bench adaptive --dir data --seed ${SEED} --ops 100000 --unit 1000 --ndb 10 +RTS -N16"
