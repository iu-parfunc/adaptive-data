#!/bin/bash
SEED=`date +'%s'`
stack bench adaptive-hashmap:pldi-bench --benchmark-arguments "--bench ctrie    --dir data --seed ${SEED} --ops 100000 --unit 1000 --iratio 0.9 --ndb 6 +RTS -N16 -qa"
#stack bench adaptive-hashmap:pldi-bench --benchmark-arguments "--bench gz       --dir data --seed ${SEED} --ops 100000 --unit 1000 --iratio 0.9 --ndb 6 +RTS -N16 -qa"
stack bench adaptive-hashmap:pldi-bench --benchmark-arguments "--bench adaptive --dir data --seed ${SEED} --ops 100000 --unit 1000 --iratio 0.9 --ndb 6 +RTS -N16 -qa"
