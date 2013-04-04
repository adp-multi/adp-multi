{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-} -- needed for Parseable
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module ADP.Multi.ElementaryParsers (
    string,
    string2,
    empty1,
    empty2,
    anychar,
    anycharExcept,
    anychar2,
    char,
    char2,
    charLeftOnly,
    charRightOnly,
    EPS(..)
) where

import Data.Array
import Data.Typeable
import Data.Data
import ADP.Multi.Parser

data EPS = EPS deriving (Eq, Show, Data, Typeable)


-- # elementary parsers for dimension 1 and 2

string' :: Eq a => [a] -> Parser a [a]
string' s z [i,j] =
    [ s | 
      j-i == length s && 
      all (\i' -> z!i' == s !! (i'-i-1)) [i+1..j] 
    ]

string :: Eq a => [a] -> RichParser a [a]
string s = 
    (
        ParserInfo1 {minYield=length s, maxYield=Just (length s)},
        string' s
    )

string2' :: Eq a => [a] -> [a] -> Parser a ([a],[a])
string2' s1 s2  z [i,j,k,l] = 
    [ (s1,s2) |
      j-i == length s1 && all (\i' -> z!i' == s1 !! (i'-i-1)) [i+1..j] &&
      l-k == length s2 && all (\k' -> z!k' == s2 !! (k'-k-1)) [k+1..l]
    ]

string2 :: Eq a => [a] -> [a] -> RichParser a ([a],[a])
string2 s1 s2 = 
    (
        ParserInfo2 {minYield2=(length s1,length s2), maxYield2=(Just (length s1),Just (length s2))},
        string2' s1 s2
    ) 

empty1' :: Parser a EPS
empty1' _ [i,j] = [ EPS | i == j ]

empty1 :: RichParser a EPS
empty1 = (
              ParserInfo1 {minYield=0, maxYield=Just 0},
              empty1'
         )

empty2' :: Parser a (EPS,EPS)
empty2' _ [i,j,k,l] = [ (EPS,EPS) | i == j && k == l ]

empty2 :: RichParser a (EPS,EPS)
empty2 = (
              ParserInfo2 {minYield2=(0,0), maxYield2=(Just 0,Just 0)},
              empty2'
         )

anychar' :: Parser a a
anychar' z [i,j] = [ z!j | i+1 == j ]

anychar :: RichParser a a
anychar = (
              ParserInfo1 {minYield=1, maxYield=Just 1},
              anychar'
          )

anychar2' :: Parser a (a,a)
anychar2' z [i,j,k,l] = [ (z!j, z!l) | i+1 == j && k+1 == l ]

anychar2 :: RichParser a (a,a)
anychar2 = (
                ParserInfo2 {minYield2=(1,1), maxYield2=(Just 1,Just 1)},
                anychar2'
           )

anycharExcept' :: Eq a => [a] -> Parser a a
anycharExcept' e z [i,j] = [ z!j | i+1 == j && z!j `notElem` e ]

anycharExcept :: Eq a => [a] -> RichParser a a
anycharExcept e = (
                      ParserInfo1 {minYield=1, maxYield=Just 1},
                      anycharExcept' e
                  )
     
char' :: Eq a => a -> Parser a a
char' c z [i,j] = [ z!j | i+1 == j && z!j == c ]
      
char :: Eq a => a -> RichParser a a
char c = (
             ParserInfo1 {minYield=1, maxYield=Just 1},
             char' c
         ) 
              
char2' ::  Eq a => a -> a -> Parser a (a,a)
char2' c1 c2 z [i,j,k,l] = 
    [ (z!j, z!l) |
      i+1 == j && k+1 == l && z!j == c1 && z!l == c2
    ]

char2 :: Eq a => a -> a -> RichParser a (a,a)
char2 c1 c2 = (
                  ParserInfo2 {minYield2=(1,1), maxYield2=(Just 1,Just 1)},
                  char2' c1 c2
              ) 
             
charLeftOnly' :: Eq a => a -> Parser a (a,EPS)
charLeftOnly' c z [i,j,k,l] = 
    [ (c, EPS) | i+1 == j && k == l && z!j == c ]
      
charLeftOnly :: Eq a => a -> RichParser a (a,EPS)
charLeftOnly c = (
                     ParserInfo2 {minYield2=(1,0), maxYield2=(Just 1,Just 0)},
                     charLeftOnly' c
                 )
                 
charRightOnly' :: Eq a => a -> Parser a (EPS,a)
charRightOnly' c z [i,j,k,l] =
    [ (EPS, c) | i == j && k+1 == l && z!l == c ]

charRightOnly :: Eq a => a -> RichParser a (EPS,a)
charRightOnly c = (
                      ParserInfo2 {minYield2=(0,1), maxYield2=(Just 0,Just 1)},
                      charRightOnly' c
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
    toParser (s1,s2) = string2 s1 s2

-- and some specific ones for chars
-- these can't be made generic as it would lead to `Parseable a a a` which is 
-- in conflict to all other instances
instance Parseable Char Char Char where
    toParser = char
    
instance Parseable (Char,Char) Char (Char,Char) where
    toParser (c1,c2) = char2 c1 c2
           
instance Parseable (EPS,Char) Char (EPS,Char) where
    toParser (_,c) = charRightOnly c
    
instance Parseable (Char,EPS) Char (Char,EPS) where
    toParser (c,_) = charLeftOnly c