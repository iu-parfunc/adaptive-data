

[2016.06.25] {Trouble reproducing a build from earlier}
-------------------------------------------------------

Commit d2ef259e696c4c8c83be4085dd22f43946b9ecc8 was building before
without a glitch.  But now a simple `stack build` yields:

    adaptive-data/adaptive-data/deps/haskell-lockfree/atomic-primops/Data/Atomics.hs:49:1: error:
        Failed to load interface for ‘Data.Primitive.Array’
        There are files missing in the ‘primitive-0.6.1.0’ package,
        try running 'ghc-pkg check'.
        Use -v to see a list of the files searched for.

Let's see if wiping out the .stack-work directories does the trick.


[2016.06.25] {Portability problem for CNF / Mac OS}
---------------------------------------------------

It looks like CNF branch of GHC in fact will not build on mac ATM:

    rts/sm/CNF.c:33:10: fatal error: 'endian.h' file not found
    #include <endian.h>


[2016.06.25] {Portability problem for Docker / Mac OS}
------------------------------------------------------

Building with docker works on a linux host.  But on Mac it seems to
erroneously try to go to the global config:

    $ stack build --docker
    Run from outside a project, using implicit global project config
    Using resolver: lts-6.4 from implicit global project's config file: ...

If I manually specify the same resolver as resides in ./stack.yaml:

    stack build --docker --resolver nightly-2016-06-21

then it still makes reference to the global config and fails:

    Compiler version mismatched, found ghc-8.1.20160531 (x86_64), but
    expected minor version match with ghc-8.0.1 (x86_64) (based on
    resolver setting in /Users/rrnewton/.stack/global/stack.yaml).

Even adding `skip-ghc-check: true` to the yaml doesn't make this go
away, because it seems no to be successfully reading the local stack.yaml.

This confirms that there's a silent working dir mounting failure:

    $ stack --docker --resolver nightly-2016-06-21 --skip-ghc-check exec find
    Run from outside a project, using implicit global project config
    Using resolver: nightly-2016-06-21 specified on command line
    .
    ./.stack-work
    ./.stack-work/docker
    ./.stack-work/docker/_home

The same command on linux lists everything in the working dir.


[2016.07.05] {Playing with a background Haskell workload + Bench1}
------------------------------------------------------------------

The background workload creates GC work, which changes how the foreground benchmark 
behaves.  Tried two things:

 * reverse a list of length 10^6 repeatadly
 * rotate a Data.Sequence (chop front and it becomes back)


Here are the reverse results:
----------------------------------------------------------------------

OOPS, the runs below restricted the key range to 1M keys.  So the
data structure is not ~= to the number of writes, which is something
like 6M.

Full output for the precompute=false versions:

     Running variant: pc-adaptive
      Running threads = 15
    Progress: 1/2
    (cold:perthread,ops1/ops2 447024 12963696)
    coldPhase: mutable phase done, transitioning...
      (trans 0.805422730743885) 
    (size 972270, stateAfterTrans B) (cold:perthread,ops1/ops2 447024
      12963696)
    coldPhase: mutable phase done, transitioning...
      (trans 0.7851662188768387) 
    (size 972270, stateAfterTrans B) (cold:perthread,ops1/ops2 447024
      12963696)
    coldPhase: mutable phase done, transitioning...
      (trans 0.7252227105200291) 
    (size 972270, stateAfterTrans B) 
      Time reported: 12.231113586574793, cycles: 28131437942, numGcs: 9085, allocated: 15634075416, gcCpuSeconds: 196.98447343299995, copied: 1127601812
     261,335,582,696 bytes allocated in the heap
     167,227,419,744 bytes copied during GC
         115,936,016 bytes maximum residency (643 sample(s))
          10,422,272 bytes maximum slop
                 352 MB total memory in use (0 MB lost due to
      fragmentation)

                                         Tot time (elapsed)  Avg pause
                                         Max pause
      Gen  0     93538 colls, 93538 par   1174.782s  65.471s     0.0007s
                                         0.0287s
      Gen  1       643 colls,   642 par   1177.485s  65.532s     0.1019s
                                         0.1669s

      Parallel GC work balance: 2.15% (serial 0%, perfect 100%)

      TASKS: 38 (1 bound, 37 peak workers (37 total), using -N18)

      SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

      INIT    time    0.004s  (  0.003s elapsed)
      MUT     time  268.247s  ( 16.472s elapsed)
      GC      time  2352.268s  (131.003s elapsed)
      EXIT    time    0.023s  (  0.024s elapsed)
      Total   time  2620.546s  (147.501s elapsed)

      Alloc rate    974,236,007 bytes per MUT second

      Productivity  10.2% of total user, 181.9% of total elapsed


And here's the pure version:

    Benchmark:     "cold"
    Variants:      ["pure"]
    Ratio:         30
    MinThreads:    15
    MaxThreads:    15
    Key range:     1000000
    Precompute:    False

     Running variant: pure
      Running threads = 15
    (cold:perthread,ops1/ops2 447024 12963696)
    coldPhase: mutable phase done, transitioning...
      (trans 9.328126907348633e-6) 
    (size 972295, stateAfterTrans _) (cold:perthread,ops1/ops2 447024
      12963696)
    coldPhase: mutable phase done, transitioning...
      (trans 2.4549663066864014e-6) 
    (size 972295, stateAfterTrans _) (cold:perthread,ops1/ops2 447024
      12963696)
    coldPhase: mutable phase done, transitioning...
      (trans 2.4139881134033203e-6) 
    (size 972295, stateAfterTrans _) 
      Time reported: 11.088797435164452, cycles: 25504123088, numGcs: 8182, allocated: 15162227304, gcCpuSeconds: 177.40480330000014, copied: 13,107,815,208
     258,936,572,304 bytes allocated in the heap
     174,592,744,752 bytes copied during GC
         114,945,984 bytes maximum residency (638 sample(s))
          10,585,696 bytes maximum slop
                 352 MB total memory in use (0 MB lost due to
      fragmentation)

                                         Tot time (elapsed)  Avg pause
                                         Max pause
      Gen  0     92101 colls, 92101 par   1144.481s  63.747s     0.0007s
                                         0.0200s
      Gen  1       638 colls,   637 par   1187.160s  66.064s     0.1035s
                                         0.1644s

      Parallel GC work balance: 2.60% (serial 0%, perfect 100%)

      TASKS: 38 (1 bound, 37 peak workers (37 total), using -N18)

      SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

      INIT    time    0.005s  (  0.003s elapsed)
      MUT     time  218.002s  ( 13.641s elapsed)
      GC      time  2331.640s  (129.811s elapsed)
      EXIT    time    0.015s  (  0.016s elapsed)
      Total   time  2549.666s  (143.471s elapsed)

      Alloc rate    1,187,769,582 bytes per MUT second

      Productivity   8.6% of total user, 152.0% of total elapsed



--------------------------------------------------------------------------
And then the Data.Sequence ones... It doesn't slow down the benchmark
nearly as much as the reverse example.

     Running variant: pc-adaptive
      Running threads = 15
    (cold:perthread,ops1/ops2 447024 12963696)
    coldPhase: mutable phase done, transitioning...
      (trans 0.7939106374979019) 
    (size 972265, stateAfterTrans B) (cold:perthread,ops1/ops2 447024
      12963696)
    coldPhase: mutable phase done, transitioning...
      (trans 0.7841235511004925) 
    (size 972265, stateAfterTrans B) (cold:perthread,ops1/ops2 447024
      12963696)
    coldPhase: mutable phase done, transitioning...
      (trans 0.759719293564558) 
    (size 972265, stateAfterTrans B) 
      Time reported: 1.7232187911868095, cycles: 3963393074, numGcs: 9212,
      allocated: 15676469320, gcCpuSeconds: 12.28045214299999, copied:
      574950296
     274,574,069,736 bytes allocated in the heap
      35,210,274,832 bytes copied during GC
         113,902,584 bytes maximum residency (131 sample(s))
           4,601,168 bytes maximum slop
                 338 MB total memory in use (0 MB lost due to
      fragmentation)

                                         Tot time (elapsed)  Avg pause
                                         Max pause
      Gen  0     121451 colls, 121451 par   396.246s  22.079s     0.0002s
                                         0.0256s
      Gen  1       131 colls,   130 par   26.096s   1.478s     0.0113s
                                         0.0194s

      Parallel GC work balance: 7.60% (serial 0%, perfect 100%)

      TASKS: 38 (1 bound, 37 peak workers (37 total), using -N18)

      SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

      INIT    time    0.004s  (  0.003s elapsed)
      MUT     time  256.048s  ( 15.663s elapsed)
      GC      time  422.342s  ( 23.557s elapsed)
      EXIT    time    0.024s  (  0.029s elapsed)
      Total   time  678.422s  ( 39.251s elapsed)

      Alloc rate    1,072,351,863 bytes per MUT second

      Productivity  37.7% of total user, 652.4% of total elapsed

-------------------------------------------------------------------

Going back to the list-reverse version, here is a briefer summary:

  cnf:  Time reported: 12.231113586574793, cycles: 28131437942, numGcs: 9085, allocated: 15,634,075,416, gcCpuSeconds: 196.98447343299995, copied: 11,276,018,512
  pure: Time reported: 11.088797435164452, cycles: 25504123088, numGcs: 8182, allocated: 15,162,227,304, gcCpuSeconds: 177.40480330000014, copied: 13,107,815,208

These numbers should reflect ONLY for the cold phase.
For reference, here are the precompute=true versions:

  cnf:  Time reported: 0.23397276923060417, cycles: 538140853, numGcs: 1, allocated: 542,712, gcCpuSeconds: 0.13198241200007033,   copied: 570,448
  pure: Time reported: 0.19674938544631004, cycles: 452525141, numGcs: 1, allocated: 542,928, gcCpuSeconds: 1.1901369000042905e-2, copied: 997,608

This seems surprising!  How can numGCs be only 1 when we are still
running that crazy allocating background thread (reversing a list)?
And why is GC slower in CNF even  though it copies fewer bytes.

Ok, one more try to tweak the benchmark.  We found a place in
Compact.PureMap's "get" where there were some non-strict function
calls and may have been thunks.  Running again with precompute=true:

  cnf:  Time reported: 30.36852427199483,  cycles: 69847288614, numGcs: 13026, allocated: 16136577848, gcCpuSeconds: 508.3951978240002, copied: 21,125,538,664
  pure: Time reported: 33.825190771371126, cycles: 77797588694, numGcs: 14021, allocated: 16656209160, gcCpuSeconds: 567.7881035549999, copied: 23,132,503,216

WHAT!?  It was precompute=true, but now the numbers shot way back up compared to the run abve with "one" GC.






[2016.07.06] {Working on parallel freezing and possibly converting}
-------------------------------------------------------------------

Parallel freezing works fine.

About to add the freezing->frozen step to it.  Before that, running
with SIZE=10M gives:

    Set numCap to 4
    Fill map, first round, time: 5.677610912942328
    Freeze, time: 0.17408160504419357
    One FreezeRandBottom, time: 0.3177041580202058
    Par FreezeRandBottom, time: 9.450231201481074e-2

That's from veronica, not in a totally quiet state.
Ok with the two phase freezing/frozen modification, it still gets a
reasonable speedup:

    Fill map, first round, time: 7.328612233977765
    Freeze, time: 0.17265749897342175
    One FreezeRandBottom, time: 0.30203769891522825
    Par FreezeRandBottom, time: 8.890546602196991e-2

Next, let's see about freezeRandConvert
---------------------------------------

Here's the horrible current, quadratic behavior on a mere 50K
elements:

    Sequential freezeFold convert, time: 4.939226899296045e-3
    One-thread Freeze+Convert bottom-up, time: 3.7561008270131424

Some of that bad behavior is due to the space leak... it's piling up
thunks inside that IORef.  But perf is about the same with
atomicModifiyIORef', so maybe it must be adequately forcing them:

    Sequential freezeFold convert, time: 4.918021964840591e-3
    One-thread Freeze+Convert bottom-up, time: 3.7141220229677856

If we change things around and instead write to the IORef accumulator
on each CHILD computation... and then bottom out to sequential after
the first level....  Then our 50K number changes to:

    Sequential freezeFold convert, time: 4.902540007606149e-3
    One-thread Freeze+Convert bottom-up, time: 5.3543890826404095e-3

And 100K:
    Sequential freezeFold convert, time: 1.5236729057505727e-2
    One-thread Freeze+Convert bottom-up, time: 1.6321051982231438e-2

500K:
    Sequential freezeFold convert, time: 0.11509568395558745
    One-thread Freeze+Convert bottom-up, time: 0.11698334303218871
    
1M:
    Sequential freezeFold convert, time: 0.23778499802574515
    One-thread Freeze+Convert bottom-up, time: 0.23906447493936867

That's not bad.  

Next, fix the child branches to check-before-diving-in
--------------------------------------------------------------------------------

This doesn't damage the time much and should make it ready for parallelism:

1M, 4 threads:
    Sequential freezeFold convert, time: 0.2446749280206859
    One-thread Freeze+Convert bottom-up, time: 0.2415562419919297


Now we're ready to try parallelizing it.
--------------------------------------------------------------------------------

Wow, I've been running with -N4 till now and I think it was getting
some parallel GC speedup even with single threaded operations.

1M, 1 thread:
    Sequential freezeFold convert, time: 0.468330965959467
    One-thread Freeze+Convert bottom-up, time: 0.460888407076709
    Parallel Freeze+Convert bottom-up, time: 0.49554094299674034

1M, 2 threads:
    Sequential freezeFold convert, time: 0.23693331400863826
    One-thread Freeze+Convert bottom-up, time: 0.23825362301431596
    Parallel Freeze+Convert bottom-up, time: 0.1591487240511924

1M, 3 threads:
    Sequential freezeFold convert, time: 0.31619791698176414
    One-thread Freeze+Convert bottom-up, time: 0.31473778502549976
    Parallel Freeze+Convert bottom-up, time: 0.16966514999512583

1M, 4 threads:
    Sequential freezeFold convert, time: 0.24210163799580187
    One-thread Freeze+Convert bottom-up, time: 0.2428661520825699
    Parallel Freeze+Convert bottom-up, time: 0.12390537594910711

There you have it.  I'm not sure what's happening at 2 vs 3 threads.
It is a randomized algorithm of course.

Just for fun, here's a 2X oversubscribed version, 8 threads:

    Sequential freezeFold convert, time: 0.3017393338959664
    One-thread Freeze+Convert bottom-up, time: 0.2780981119722128
    Parallel Freeze+Convert bottom-up, time: 0.17751108191441745

It doesn't go crazy.
