#!/bin/bash
set -o xtrace
.stack-work/dist/x86_64-linux/Cabal-1.24.0.0/build/pldi-bench/pldi-bench --ops 160 --dir data --bench ctrie +RTS -N1 -s
.stack-work/dist/x86_64-linux/Cabal-1.24.0.0/build/pldi-bench/pldi-bench --ops 160 --dir data --bench ctrie +RTS -N16 -s
.stack-work/dist/x86_64-linux/Cabal-1.24.0.0/build/pldi-bench/pldi-bench --ops 160 --dir data --bench gz +RTS -N1 -s
.stack-work/dist/x86_64-linux/Cabal-1.24.0.0/build/pldi-bench/pldi-bench --ops 160 --dir data --bench gz +RTS -N16 -s
