{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-} -- needed for Parseable
{-# LANGUAGE DeriveDataTypeable #-}

module ADP.Multi.SimpleParsers where

import Data.Array
import Data.Typeable
import Data.Data
import ADP.Multi.Parser

data EPS = EPS deriving (Eq, Show, Data, Typeable)


-- # elementary parsers

empty1 :: RichParser a EPS
empty1 = (
              ParserInfo1 {minYield=0, maxYield=Just 0},
              \ _ [i,j] -> 
                [ EPS |
                  i == j
                ]
         )

empty2 :: RichParser a (EPS,EPS)
empty2 = (
              ParserInfo2 {minYield2=(0,0), maxYield2=(Just 0,Just 0)},
              \ _ [i,j,k,l] -> 
                [ (EPS,EPS) |
                  i == j && k == l
                ]
         )

anychars :: RichParser a (a,a)
anychars = (
                ParserInfo2 {minYield2=(1,1), maxYield2=(Just 1,Just 1)},
                \ z [i,j,k,l] -> 
                        [ (z!j, z!l) |
                          i+1 == j && k+1 == l
                        ]
           )

chars :: Eq a => a -> a -> RichParser a (a,a)
chars c1 c2 = (
                  ParserInfo2 {minYield2=(1,1), maxYield2=(Just 1,Just 1)},
                  \ z [i,j,k,l] -> 
                        [ (z!j, z!l) |
                          i+1 == j && k+1 == l && z!j == c1 && z!l == c2
                        ]
              ) 
              
char :: Eq a => a -> RichParser a a
char c = (
                  ParserInfo1 {minYield=1, maxYield=Just 1},
                  \ z [i,j] -> 
                        [ (z!j) |
                          i+1 == j && z!j == c
                        ]
              ) 
              
anychar :: RichParser a a
anychar = (
                  ParserInfo1 {minYield=1, maxYield=Just 1},
                  \ z [i,j] -> 
                        [ (z!j) |
                          i+1 == j
                        ]
              ) 
        
charLeftOnly :: Eq a => a -> RichParser a (a,EPS)
charLeftOnly c = (
                     ParserInfo2 {minYield2=(1,0), maxYield2=(Just 1,Just 0)},
                     \ z [i,j,k,l] -> 
                        [ (c, EPS) |
                          i+1 == j && k == l && z!j == c
                        ]
                 )

charRightOnly :: Eq a => a -> RichParser a (EPS,a)
charRightOnly c = (
                      ParserInfo2 {minYield2=(0,1), maxYield2=(Just 0,Just 1)},
                      \ z [i,j,k,l] -> 
                        [ (EPS, c) |
                          i == j && k+1 == l && z!l == c
                        ]
                  )
    
-- # some syntax sugar

instance Parseable EPS Char EPS where
    toParser _ = empty1

instance Parseable Char Char Char where
    toParser = char

instance Parseable (EPS,EPS) Char (EPS,EPS) where
    toParser _ = empty2

instance Parseable (Char,Char) Char (Char,Char) where
    toParser (c1,c2) = chars c1 c2
    
instance Parseable (EPS,Char) Char (EPS,Char) where
    toParser (_,c) = charRightOnly c
    
instance Parseable (Char,EPS) Char (Char,EPS) where
    toParser (c,_) = charLeftOnly c