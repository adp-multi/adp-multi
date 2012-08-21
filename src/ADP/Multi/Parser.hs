{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module ADP.Multi.Parser where

import Data.Array

type Subword2  = (Int,Int,Int,Int)
type Parser2 a b = Array Int a -> Subword2 -> [b]

data ParserInfo2 = ParserInfo2 
                   {
                      minYield :: (Int,Int)
                   ,  maxYield :: (Maybe Int,Maybe Int) 
                   }
                   deriving Show
                   
type RichParser2 a b = (ParserInfo2, Parser2 a b)

class Parseable p a b | p -> a b where
    toParser :: p -> RichParser2 a b
    
instance Parseable (RichParser2 a b) a b where
    toParser p = p
