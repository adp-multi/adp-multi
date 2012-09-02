module ADP.Multi.Tabulation where

import Data.Array
import ADP.Multi.Parser


-- four-dimensional tabulation
table2 :: Array Int a -> RichParser2 a b -> RichParser2 a b
table2 z (info,q) =
   let (_,n) = bounds z
       arr = array ((0,0,0,0),(n,n,n,n))
                      [ ((i,j,k,l),q z (i,j,k,l)) |
                        i <- [0..n]
                      , j <- [i..n]
                      , k <- [0..n]
                      , l <- [k..n]
                      ]
   in (info, \ _ -> (!) arr )
        
-- TODO tabulation with diagonal arrays