adp-multi
=========

[![Build Status](https://secure.travis-ci.org/neothemachine/adp-multi.png?branch=master)](http://travis-ci.org/neothemachine/adp-multi)

Work in progress!

Some notes...
-------------

run with +RTS -xc to get stack traces on exceptions
- has to be compiled with -rtsopts -prof -auto-all (performance will be degraded)
- packages have to be installed with --enable-library-profiling
  (can be enabled by default in ~/.cabal/config or ~\AppData\Roaming\cabal\config, respectively)

run with +RTS -N to enable multicore support (has to be compiled with -threaded)
(at the moment it runs faster single-threaded)