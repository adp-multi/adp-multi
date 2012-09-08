module ADP.Multi.Rewriting where

import ADP.Multi.Parser

data Ranges = RangeMap Subword [Ranges] deriving Show

type YieldAnalysisAlgorithm a = a -> [ParserInfo] -> ParserInfo
type RangeConstructionAlgorithm a = a -> [ParserInfo] -> Subword -> [Ranges]

type Dim1 = [(Int, Int)] -> [(Int, Int)] 
type Dim2 = [(Int, Int)] -> ([(Int, Int)], [(Int, Int)]) 

-- | Convenience function for one dim2 symbol
id2 :: [a] -> ([a], [a])
id2 [c1,c2] = ([c1],[c2])
id2 _ = error "Only use id2 for single symbols! Write your own rewrite function instead."