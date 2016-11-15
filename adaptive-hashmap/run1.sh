#!/bin/bash
SEED=`date +'%s'`
stack bench adaptive-hashmap:set-bench --benchmark-arguments "--bench ctrie     --seed ${SEED} --ops 500 --unit 500 --hratio 0.5 --iratio 0.9 --ndb 5000 --range 8192 +RTS -N16 -qa -s"
stack bench adaptive-hashmap:set-bench --benchmark-arguments "--bench bset      --seed ${SEED} --ops 500 --unit 500 --hratio 0.5 --iratio 0.9 --ndb 5000 --range 8192 +RTS -N16 -qa -s"
stack bench adaptive-hashmap:set-bench --benchmark-arguments "--bench adaptive  --seed ${SEED} --ops 500 --unit 500 --hratio 0.5 --iratio 0.9 --ndb 5000 --range 8192 +RTS -N16 -qa -s"
