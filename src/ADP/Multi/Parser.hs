{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module ADP.Multi.Parser where

import Data.Array

data ParserInfo = ParserInfo1 
                   {
                      minYield :: Int
                   ,  maxYield :: Maybe Int 
                   }
                | ParserInfo2 
                   {
                      minYield2 :: (Int,Int)
                   ,  maxYield2 :: (Maybe Int,Maybe Int) 
                   }
                deriving Show

type Subword1  = (Int,Int)
type Parser1 a b = Array Int a -> Subword1 -> [b]

type Subword2  = (Int,Int,Int,Int)
type Parser2 a b = Array Int a -> Subword2 -> [b]
 
data Parser a b = P1 (Parser1 a b) 
                | P2 (Parser2 a b)
                
                
type RichParser a b = (ParserInfo, Parser a b)
                    
class Parseable p a b | p -> a b where
    toParser :: p -> RichParser a b
    
instance Parseable (RichParser a b) a b where
    toParser p = p