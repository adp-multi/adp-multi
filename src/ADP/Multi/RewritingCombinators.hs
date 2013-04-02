module ADP.Multi.RewritingCombinators where

import ADP.Debug
import ADP.Multi.Parser
import ADP.Multi.Rewriting
import ADP.Multi.Rewriting.YieldSize
import ADP.Multi.Rewriting.Explicit

infix 6 >>>|
(>>>|) :: ([ParserInfo], [Ranges] -> Parser a b) -> Dim1 -> RichParser a b
(>>>|) = rewrite determineYieldSize1 constructRanges1

infix 6 >>>||
(>>>||) :: ([ParserInfo], [Ranges] -> Parser a b) -> Dim2 -> RichParser a b
(>>>||) = rewrite determineYieldSize2 constructRanges2
           
rewrite yieldAlg rangeAlg (infos,p) f =
        let yieldSize = yieldAlg f infos
        in trace (">>> yield size: " ++ show yieldSize) $
           (
              yieldSize,
              \ z subword ->
                let ranges = rangeAlg f infos subword
                in trace (">>> " ++ show subword) $
                trace ("ranges: " ++ show ranges) $ 
                [ result |
                  RangeMap sub rest <- ranges
                , result <- p rest z sub 
                ]
           )