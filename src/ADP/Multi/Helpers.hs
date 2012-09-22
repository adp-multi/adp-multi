module ADP.Multi.Helpers where

import Control.Exception
import Data.Array
import ADP.Multi.Parser

axiom :: Array Int a -> RichParser a b -> [b]
axiom z (_,ax) =
    let (_,l) = bounds z
    in ax z [0,l]
    

axiomTwoTrack :: Eq a => Array Int a -> [a] -> [a] -> RichParser a b -> [b]
axiomTwoTrack z inp1 inp2 (_,ax) =
    assert (z == mkTwoTrack inp1 inp2) $
    ax z [0,l1,l1,l1+l2]
    where l1 = length inp1
          l2 = length inp2

        
-- # Create array from List

mk :: [a] -> Array Int a
mk xs = array (1,length xs) (zip [1..] xs)

mkTwoTrack :: [a] -> [a] -> Array Int a
mkTwoTrack xs ys = mk (xs ++ ys)