{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Some common types for parsers. 
module ADP.Multi.Parser where

import Data.Array

-- | To support higher dimensions, a subword is a list
--   of indices. Valid list lengths are 2n with n>0.
type Subword = [Int]

type Parser a b = Array Int a   -- ^ The input sequence
               -> Subword       -- ^ Subword of the input sequence to be parsed
               -> [b]           -- ^ Parsing results

-- | Static information about yield sizes of a parser.
--   For supporting dimensions > 2, this type has to be
--   expanded with more constructors, or redesigned to be generic.
data ParserInfo = ParserInfo1 {
                    minYield :: Int
                  , maxYield :: Maybe Int
                  }
                | ParserInfo2 {
                    minYield2 :: (Int,Int)
                  , maxYield2 :: (Maybe Int,Maybe Int)
                  }
                deriving (Eq, Show)
                   
type RichParser a b = (ParserInfo, Parser a b)

class Parseable p a b | p -> a b where
    toParser :: p -> RichParser a b
    
instance Parseable (RichParser a b) a b where
    toParser p = p
