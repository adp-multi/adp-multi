module ADP.Multi.RewritingCombinatorsConstraint where

import ADP.Multi.Parser
import ADP.Multi.Rewriting
import ADP.Multi.RewritingCombinators(rewrite)
import ADP.Multi.Rewriting.YieldSize
import ADP.Multi.Rewriting.ConstraintSolver

infix 6 >>>|
(>>>|) :: ([ParserInfo], [Ranges] -> Parser a b) -> Dim1 -> RichParser a b
(>>>|) = rewrite determineYieldSize1 constructRanges1

infix 6 >>>||
(>>>||) :: ([ParserInfo], [Ranges] -> Parser a b) -> Dim2 -> RichParser a b
(>>>||) = rewrite determineYieldSize2 constructRanges2