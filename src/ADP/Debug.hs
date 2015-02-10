{-# LANGUAGE CPP #-}

-- | Debugging is enabled via the cabal flag /DEBUG/
module ADP.Debug where

import Debug.HTrace (htrace)

trace :: String -> a -> a
#ifdef ADPDEBUG
trace = htrace
#else
trace _ b = b
#endif