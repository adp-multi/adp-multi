module Criterion.Helpers where

import Criterion.Main
import Control.DeepSeq (NFData)

benchArray :: NFData b => (a -> b) -> [(String,a)] -> [Benchmark]
benchArray fun args =
    [bench name (nf fun arg) | (name,arg) <- args]