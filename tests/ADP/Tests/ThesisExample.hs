-- | Example code corresponding to section 6.1 of the thesis.
--   The same but using signatures, products, and more algebras
--   can be found in RGExample*.hs (variable names are different).
module ADP.Tests.ThesisExample where

import ADP.Multi.All
import ADP.Multi.Rewriting.All hiding (id1,id2)

-- rewriting functions
id1,r0,r1,r2,r3 :: Dim1
id2,r4          :: Dim2

id1 [x]                          = [x]
id2 [x1,x2]                      = ([x1],[x2])
r0 [e]                           = [e]
r1 [b,z]                         = [b,z]
r2 [p1,p2,z1,z2]                 = [p1,z1,p2,z2]
r3 [m11,m12,m21,m22,z1,z2,z3,z4] = [m11,z1,m21,z2,m12,z3,m22,z4]
r4 [p1,p2,m1,m2]                 = ([m1,p1],[p2,m2])

-- evaluation algebra for terms
data Term  = F1 Term Term
           | F2 Term Term Term
           | F3 String
           | F4 Term Term Term Term Term Term
           | F5 Term Term
           | F6 Term
           | F7 (String,String)
           | F8 String
           deriving (Eq, Show)
(f1,f2,f3,f4,f5,f6,f7,f8) = (F1,F2,F3,F4,F5,F6,F7,F8)
h = id

-- or alternatively: evaluation algebra for counting base pairs
--f1 b z               = z
--f2 p z1 z2           = p + z1 + z2
--f3 _                 = 0
--f4 m1 m2 z1 z2 z3 z4 = m1 + m2 + z1 + z2 + z3 + z4
--f5 m p               = m + p
--f6 p                 = p
--f7 _                 = 1
--f8 _                 = 0
--h [] = []
--h xs = [maximum xs]

-- input
w = "agcguu"
w' = mk w

-- memoization
tabulated1 = table1 w'
tabulated2 = table2 w'

-- grammar productions
z = tabulated1 $
    yieldSize1 (0, Nothing) $
    f1 <<< b ~~~ z                          >>> r1 |||
    f2 <<< p ~~~ z ~~~ z                    >>> r2 |||
    f3 <<< ""                               >>> r0 |||
    f4 <<< m ~~~  m ~~~ z ~~~ z ~~~ z ~~~ z >>> r3
    ... h
        
m = tabulated2 $
    yieldSize2 (1, Nothing) (1, Nothing) $
    f5 <<< m ~~~ p >>> r4  |||
    f6 <<< p       >>> id2
    ... h

p = tabulated2 $
    f7 <<< ("a","u") >>> id2 |||
    f7 <<< ("u","a") >>> id2 |||
    f7 <<< ("c","g") >>> id2 |||
    f7 <<< ("g","c") >>> id2 |||
    f7 <<< ("g","u") >>> id2 |||
    f7 <<< ("u","g") >>> id2
    ... h
            
b = tabulated1 $
    f8 <<< "a" >>> id1 |||
    f8 <<< "u" >>> id1 |||
    f8 <<< "c" >>> id1 |||
    f8 <<< "g" >>> id1
    ... h

-- result
(staticInfoZ,parserZ) = z
result = parserZ w' [0,length w]
