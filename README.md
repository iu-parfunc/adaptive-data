# adaptive-data

## Build status:

* Travis: [![Build Status](https://travis-ci.org/iu-parfunc/adaptiv-data.svg?branch=master)](https://travis-ci.org/iu-parfunc/adaptive-data/)

 * Jenkins: [![Build Status](http://tester-lin.soic.indiana.edu:8080/buildStatus/icon?job=adaptive_data_chen)](http://tester-lin.soic.indiana.edu:8080/job/adaptive_data_chen/)

## Description


Various experiments in adaptive data structures.

Here's a breakdown of the top-level directories:

 * adaptive-hashmap -- our most recent prototype [2016.03.05] that
   uses CTrie and HashMap from unordered-containers.
  
 * adaptive_map_prototype -- 
 * chen_bag --

Plus, experiments from the ICFP'15 paper:

 * j_benchmark --
 * old_peter_bag_test --  

Plus various dependencies:
 (TODO: move these to a `deps/` subdir)

 * haskell-lockfree
 * concurrent-skiplist
 * pcg-random
