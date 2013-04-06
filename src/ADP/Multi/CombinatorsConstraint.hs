{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{-
Use this set of combinators instead of ADP.Multi.Combinators to
use a constraint solver for constructing the indices ranges.

Note: This is experimental and slow.
-} 
module ADP.Multi.CombinatorsConstraint (
    (<<<),
    (~~~),
    yieldSize1, yieldSize2,
    (>>>),
    (|||),
    (...),
    with
) where

import ADP.Multi.Parser
import ADP.Multi.Rewriting
import ADP.Multi.Combinators hiding ((>>>))
import ADP.Multi.Rewriting.YieldSize
import ADP.Multi.Rewriting.ConstraintSolver

class Rewritable r a b where
    infix 6 >>>
    (>>>) :: ([ParserInfo], [Ranges] -> Parser a b) -> r -> RichParser a b

instance Rewritable Dim1 a b where
    (>>>) (infos,p) f = 
      (determineYieldSize1 f infos, rewrite constructRanges1 (infos,p) f)
    
instance Rewritable Dim2 a b where
    (>>>) (infos,p) f = 
      (determineYieldSize2 f infos, rewrite constructRanges2 (infos,p) f)