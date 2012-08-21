module ADP.Debug where

import Debug.HTrace (htrace)

trace _ b = b
--trace = htrace