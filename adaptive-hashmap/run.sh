#!/bin/bash
SEED=`date +'%s'`
stack bench adaptive-hashmap:pldi-bench --benchmark-arguments "--bench ctrie  --dir data_small --seed ${SEED} --ops 500 --unit 500 --hratio 0.8 --iratio 0.9 --ndb 500 --range 5120 +RTS -N4 -qa -s"
stack bench adaptive-hashmap:pldi-bench --benchmark-arguments "--bench gz     --dir data_small --seed ${SEED} --ops 500 --unit 500 --hratio 0.8 --iratio 0.9 --ndb 500 --range 5120 +RTS -N4 -qa -s"
stack bench adaptive-hashmap:pldi-bench --benchmark-arguments "--bench agz    --dir data_small --seed ${SEED} --ops 500 --unit 500 --hratio 0.8 --iratio 0.9 --ndb 500 --range 5120 +RTS -N4 -qa -s"
