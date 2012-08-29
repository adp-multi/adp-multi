module ADP.Multi.Helpers where

import Data.Array
import ADP.Multi.Parser

axiom :: Array Int a -> RichParser a b -> [b]
axiom z (_,P1 ax) = 
    let (_,l) = bounds z
    in ax z (0,l)

axiom _ _ = error "dimension of start must be 1"
  

        
-- # Create array from List

mk :: [a] -> Array Int a
mk xs = array (1,length xs) (zip [1..] xs)