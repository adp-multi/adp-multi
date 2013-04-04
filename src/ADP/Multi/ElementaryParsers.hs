{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-} -- needed for Parseable
{-# LANGUAGE DeriveDataTypeable #-}

module ADP.Multi.ElementaryParsers where

import Data.Array
import Data.Typeable
import Data.Data
import ADP.Multi.Parser

data EPS = EPS deriving (Eq, Show, Data, Typeable)


-- # elementary parsers for dimension 1 and 2

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
              
anycharExcept :: Eq a => [a] -> RichParser a a
anycharExcept e = (
                  ParserInfo1 {minYield=1, maxYield=Just 1},
                  \ z [i,j] -> 
                        [ (z!j) |
                          i+1 == j && z!j `notElem` e
                        ]
              )
     
elemsSub :: Array Int a -> Int -> Int -> [a]
elemsSub arr i j = [arr!i' | i' <- [i .. j]]
              
string :: Eq a => [a] -> RichParser a [a]
string s = (
                  ParserInfo1 {minYield=length s, maxYield=Just (length s)},
                  \ z [i,j] -> 
                        [ s |
                          j-i == length s && all (\i' -> z!i' == s !! (i'-i-1)) [i+1..j]
                        ]
              )

strings :: Eq a => [a] -> [a] -> RichParser a ([a],[a])
strings s1 s2 = (
                  ParserInfo2 {minYield2=(length s1,length s2), maxYield2=(Just (length s1),Just (length s2))},
                  \ z [i,j,k,l] -> 
                        [ (s1,s2) |
                          j-i == length s1 && all (\i' -> z!i' == s1 !! (i'-i-1)) [i+1..j] &&
                          l-k == length s2 && all (\k' -> z!k' == s2 !! (k'-k-1)) [k+1..l]
                        ]
              ) 

anystringWithout :: Eq a => [a] -> RichParser a [a]
anystringWithout e = (
                  ParserInfo1 {minYield=0, maxYield=Nothing},
                  \ z [i,j] -> 
                        [ elemsSub z (i+1) j |
                          i<j && all (\i' -> z!i' `notElem` e) [i+1..j]
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

-- generic instances
instance Parseable EPS a EPS where
    toParser _ = empty1
    
instance Parseable (EPS,EPS) a (EPS,EPS) where
    toParser _ = empty2
    
instance Eq a => Parseable [a] a [a] where
    toParser = string
    
instance Eq a => Parseable ([a],[a]) a ([a],[a]) where
    toParser (s1,s2) = strings s1 s2

-- and some specific ones for chars
-- these can't be made generic as it would lead to `Parseable a a a` which is 
-- in conflict to all other instances
instance Parseable Char Char Char where
    toParser = char
    
instance Parseable (Char,Char) Char (Char,Char) where
    toParser (c1,c2) = chars c1 c2
           
instance Parseable (EPS,Char) Char (EPS,Char) where
    toParser (_,c) = charRightOnly c
    
instance Parseable (Char,EPS) Char (Char,EPS) where
    toParser (c,_) = charLeftOnly c