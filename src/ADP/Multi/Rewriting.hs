module ADP.Multi.Rewriting where

import ADP.Multi.Parser

data Ranges = Ranges1 Subword1 [Ranges] 
            | Ranges2 Subword2 [Ranges]
            deriving Show

type YieldAnalysisAlgorithm a = a -> [ParserInfo] -> ParserInfo

type RangeConstructionAlgorithm1 = Dim1 -> [ParserInfo] -> Subword1 -> [Ranges]
type RangeConstructionAlgorithm2 = Dim2 -> [ParserInfo] -> Subword2 -> [Ranges]

type Dim1 = [(Int, Int)] -> [(Int, Int)] 
type Dim2 = [(Int, Int)] -> ([(Int, Int)], [(Int, Int)]) 