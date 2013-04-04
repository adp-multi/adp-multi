module ADP.Multi.Rewriting where

import ADP.Debug
import ADP.Multi.Parser

data Ranges = RangeMap Subword [Ranges] deriving Show

type YieldAnalysisAlgorithm a = a -> [ParserInfo] -> ParserInfo
type RangeConstructionAlgorithm a = a -> [ParserInfo] -> Subword -> [Ranges]

type Dim1 = [(Int, Int)] -> [(Int, Int)] 
type Dim2 = [(Int, Int)] -> ([(Int, Int)], [(Int, Int)]) 

-- used by >>>| and >>>|| in ADP.Multi.Combinators and ADP.Multi.CombinatorsConstraint
rewrite :: YieldAnalysisAlgorithm a 
        -> RangeConstructionAlgorithm a
        -> ([ParserInfo], [Ranges] -> Parser b c) 
        -> a    -- rewriting function, Dim1 or Dim2
        -> RichParser b c
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