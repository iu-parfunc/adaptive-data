#!/bin/bash
stack bench adaptive-hashmap:pldi-bench --benchmark-arguments "--bench ctrie --dir data --seed `date +'%s'` --ops 10000 --unit 100 +RTS -N16"
stack bench adaptive-hashmap:pldi-bench --benchmark-arguments "--bench gz --dir data --seed `date +'%s'` --ops 10000 --unit 100 +RTS -N16"
