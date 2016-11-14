#!/bin/bash
SEED=`date +'%s'`
stack bench adaptive-hashmap:pldi-bench --benchmark-arguments "--bench ctrie    --dir data --seed ${SEED} --ops 100 --unit 100 --iratio 0.9 --ndb 1000 --range 5120 +RTS -N16 -qa"
stack bench adaptive-hashmap:pldi-bench --benchmark-arguments "--bench agz      --dir data --seed ${SEED} --ops 100 --unit 100 --iratio 0.9 --ndb 1000 --range 5120 +RTS -N16 -qa"
