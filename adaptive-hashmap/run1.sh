#!/bin/bash
SEED=`date +'%s'`
stack bench adaptive-hashmap:set-bench --benchmark-arguments "--bench ctrie --seed ${SEED} --ops 1000 --unit 1000 --iratio 0.9 --ndb 5000 --range 65536 +RTS -N16 -qa -s"
stack bench adaptive-hashmap:set-bench --benchmark-arguments "--bench bset  --seed ${SEED} --ops 1000 --unit 1000 --iratio 0.9 --ndb 5000 --range 65536 +RTS -N16 -qa -s"
