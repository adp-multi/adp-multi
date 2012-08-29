module ADP.Multi.Rewriting where

import ADP.Multi.Parser

data Ranges = RangeMap Subword [Ranges] deriving Show

type YieldAnalysisAlgorithm a = a -> [ParserInfo] -> ParserInfo
type RangeConstructionAlgorithm a = a -> [ParserInfo] -> Subword -> [Ranges]

type Dim1 = [(Int, Int)] -> [(Int, Int)] 
type Dim2 = [(Int, Int)] -> ([(Int, Int)], [(Int, Int)]) 