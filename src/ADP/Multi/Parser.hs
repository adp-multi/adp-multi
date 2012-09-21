{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module ADP.Multi.Parser where

import Data.Array

type Subword = [Int]
type Parser a b = Array Int a -> Subword -> [b]

data ParserInfo = ParserInfo1
                   {
                     minYield :: Int
                   , maxYield :: Maybe Int
                   }
                | ParserInfo2
                   {
                     minYield2 :: (Int,Int)
                   , maxYield2 :: (Maybe Int,Maybe Int)
                   }
                deriving (Eq, Show)
                   
type RichParser a b = (ParserInfo, Parser a b)

class Parseable p a b | p -> a b where
    toParser :: p -> RichParser a b
    
instance Parseable (RichParser a b) a b where
    toParser p = p
