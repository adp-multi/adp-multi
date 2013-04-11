module ADP.Debug where

import Debug.HTrace (htrace)

trace :: [Char] -> a -> a
trace _ b = b
--trace = htrace