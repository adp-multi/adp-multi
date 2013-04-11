-- | Types for the rewriting combinator
module ADP.Multi.Rewriting where

import ADP.Multi.Parser

-- | Tree of subwords. Every path in a tree represents
--   a sequence of subwords for a corresponding sequence of parsers
--   in a production. 
data SubwordTree = SubwordTree Subword [SubwordTree] deriving Show

type SubwordConstructionAlgorithm a = a -> [ParserInfo] -> Subword -> [SubwordTree]