module ADP.Multi.Rewriting where

import ADP.Multi.Parser

type Subword  = (Int,Int)

data Ranges = RangeMap Subword2 [Ranges] deriving Show

class Rewriting f where
  constructRanges :: f -> [ParserInfo2] -> Subword2 -> [Ranges]
  determineYieldSize :: f -> [ParserInfo2] -> ParserInfo2 