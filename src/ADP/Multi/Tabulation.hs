-- Combinators for one- and two-dimensional tabulation
module ADP.Multi.Tabulation where

import Data.Array
import ADP.Multi.Parser

-- two-dimensional tabulation
table1' :: Array Int a -> Parser a b -> Parser a b
table1' z q = 
    let (_,n) = bounds z
        arr = array ((0,0),(n,n))
                    [ ((i,j), q z [i,j])
                    | i <- [0..n], j <- [i..n] ]
    in \ _ [i,j] -> arr ! (i,j)

table1 :: Array Int a -> RichParser a b -> RichParser a b
table1 z (info,q) = (info, table1' z q)

-- four-dimensional tabulation
table2' :: Array Int a -> Parser a b -> Parser a b
table2' z q =
    let (_,n) = bounds z
        arr = array ((0,0,0,0),(n,n,n,n))
                    [ ((i,j,k,l), q z [i,j,k,l])
                    | i <- [0..n], j <- [i..n]
                    , k <- [0..n], l <- [k..n] ]
    in \ _ [i,j,k,l] -> arr ! (i,j,k,l)

table2 :: Array Int a -> RichParser a b -> RichParser a b
table2 z (info,q) = (info, table2' z q)