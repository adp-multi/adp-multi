{-
Use this set of combinators instead of ADP.Multi.Combinators to
use a constraint solver for constructing the indices ranges.

Note: This is experimental and slow.
-} 
module ADP.Multi.CombinatorsConstraint (
    (<<<),(<<<|),(<<<||),
    (~~~),(~~~|),(~~~||),
    (>>>|),(>>>||),
    (|||),
    (...),
    with
) where

import ADP.Multi.Parser
import ADP.Multi.Rewriting
import ADP.Multi.Combinators hiding ((>>>|),(>>>||))
import ADP.Multi.Rewriting.YieldSize
import ADP.Multi.Rewriting.ConstraintSolver

infix 6 >>>|
(>>>|) :: ([ParserInfo], [Ranges] -> Parser a b) -> Dim1 -> RichParser a b
(>>>|) = rewrite determineYieldSize1 constructRanges1

infix 6 >>>||
(>>>||) :: ([ParserInfo], [Ranges] -> Parser a b) -> Dim2 -> RichParser a b
(>>>||) = rewrite determineYieldSize2 constructRanges2