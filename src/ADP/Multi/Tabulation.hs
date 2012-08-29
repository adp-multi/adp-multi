module ADP.Multi.Tabulation where

import Data.Array
import ADP.Multi.Parser


table :: Int -> RichParser a b -> RichParser a b
table n richParser = case richParser of
  RP1 (info,q) -> RP1
        (info, 
          \ z -> (!) $ array ((0,0),(n,n))
                      [ ((i,j),q z (i,j)) |
                        i<- [0..n]
                      , j<- [i..n]
                      ]
        )
  RP2 (info,q) -> RP2
        (info, 
          \ z -> (!) $ array ((0,0,0,0),(n,n,n,n))
                      [ ((i,j,k,l),q z (i,j,k,l)) |
                        i <- [0..n]
                      , j <- [i..n]
                      , k <- [0..n]
                      , l <- [k..n]
                      ]
        )
        
-- TODO tabulation with diagonal arrays