module ADP.Multi.Helpers where

import Data.Array
import ADP.Multi.Parser

axiom :: Array Int a -> RichParser a b -> [b]
axiom z (_,ax) =
    let (_,l) = bounds z
    in ax z [0,l]

        
-- # Create array from List

mk :: [a] -> Array Int a
mk xs = array (1,length xs) (zip [1..] xs)