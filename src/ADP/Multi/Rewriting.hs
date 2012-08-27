module ADP.Multi.Rewriting where

import ADP.Multi.Parser

data Ranges = RangeMap Subword2 [Ranges] deriving Show

type YieldAnalysisAlgorithm a = a -> [ParserInfo2] -> ParserInfo2
type RangeConstructionAlgorithm a = a -> [ParserInfo2] -> Subword2 -> [Ranges]

type Dim2 = [(Int, Int)] -> ([(Int, Int)], [(Int, Int)]) 