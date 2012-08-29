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

empty2 :: RichParser2 a ()
empty2 = (
              ParserInfo2 {minYield2=(0,0), maxYield2=(Just 0,Just 0)},
              \ _ (i,j,k,l) -> 
                [ () |
                  i == j && k == l
                ]
         )

anychars :: RichParser2 a (a,a)
anychars = (
                ParserInfo2 {minYield2=(1,1), maxYield2=(Just 1,Just 1)},
                \ z (i,j,k,l) -> 
                        [ (z!j, z!l) |
                          i+1 == j && k+1 == l
                        ]
           )

chars :: Eq a => a -> a -> RichParser2 a (a,a)
chars c1 c2 = (
                  ParserInfo2 {minYield2=(1,1), maxYield2=(Just 1,Just 1)},
                  \ z (i,j,k,l) -> 
                        [ (z!j, z!l) |
                          i+1 == j && k+1 == l && z!j == c1 && z!l == c2
                        ]
              ) 
        
charLeftOnly :: Eq a => a -> RichParser2 a (a,EPS)
charLeftOnly c = (
                     ParserInfo2 {minYield2=(1,0), maxYield2=(Just 1,Just 0)},
                     \ z (i,j,k,l) -> 
                        [ (c, EPS) |
                          i+1 == j && k == l && z!j == c
                        ]
                 )

charRightOnly :: Eq a => a -> RichParser2 a (EPS,a)
charRightOnly c = (
                      ParserInfo2 {minYield2=(0,1), maxYield2=(Just 0,Just 1)},
                      \ z (i,j,k,l) -> 
                        [ (EPS, c) |
                          i == j && k+1 == l && z!l == c
                        ]
                  )
    
-- # some syntax sugar

instance Parseable () a () where
    toParser _ = RP2 empty2 

instance Eq a => Parseable (a,a) a (a,a) where
    toParser (c1,c2) = RP2 $ chars c1 c2
    
instance Eq a => Parseable (EPS,a) a (EPS,a) where
    toParser (_,c) = RP2 $ charRightOnly c
    
instance Eq a => Parseable (a,EPS) a (a,EPS) where
    toParser (c,_) = RP2 $ charLeftOnly c