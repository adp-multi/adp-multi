adp-multi [![Build Status](https://secure.travis-ci.org/neothemachine/adp-multi.png?branch=master)](http://travis-ci.org/neothemachine/adp-multi)
==========

adp-multi is an adaptation of the [Algebraic Dynamic Programming](http://bibiserv.techfak.uni-bielefeld.de/adp/)
method for [multiple context-free grammars](http://adp-multi.ruhoh.com/mcfg).

It is a library based on the original [Haskell-ADP implementation](https://bitbucket.org/gsauthof/adpcombinators)
and can be considered an unoptimized prototype.

You can find out more about it at its [project page](http://adp-multi.ruhoh.com).

Quick start
-----------

1. Check out the git repository
2. Run `cabal install --flags="+buildTests"` inside the checked out folder
3. Run `adp-test` to see if there's some output (then it works)
4. Explore the code (start at `tests/ADP/Tests/Main.hs`)

How to debug errors
-------------------

If you can, write unit and/or property tests.

If you get Haskell exceptions, then it is useful to enable stack traces:

0. (If not already done) Install all used packages with `--enable-library-profiling`
   (can be enabled by default in ~/.cabal/config or ~\AppData\Roaming\cabal\config, respectively)
1. Compile adp-multi with the GHC options `-rtsopts -prof -auto-all` (performance will be degraded)
2. Run with `+RTS -xc` to get stack traces on exceptions