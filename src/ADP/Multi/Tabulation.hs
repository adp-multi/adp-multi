module ADP.Multi.Tabulation where

import Data.Array
import ADP.Multi.Parser


table :: Int -> RichParser a b -> RichParser a b
table n (info,parser) =
    (info,
     case parser of
      P1 q -> P1 $
              \ z -> (!) $ array ((0,0),(n,n))
                          [ ((i,j),q z (i,j)) |
                            i<- [0..n]
                          , j<- [i..n]
                          ]
      P2 q -> P2 $
              \ z -> (!) $ array ((0,0,0,0),(n,n,n,n))
                          [ ((i,j,k,l),q z (i,j,k,l)) |
                            i <- [0..n]
                          , j <- [i..n]
                          , k <- [0..n]
                          , l <- [k..n]
                          ]
    )
        
-- TODO tabulation with diagonal arrays