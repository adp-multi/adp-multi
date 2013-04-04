module ADP.Multi.Helpers (
    axiom,
    axiomTwoTrack,
    mk,
    mkTwoTrack,
    id1,id2,
    Dim1,Dim2
) where

import Control.Exception
import Data.Array
import ADP.Multi.Parser
import ADP.Multi.Rewriting(Dim1,Dim2)

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


-- | Convenience rewriting function for one or more dim1 symbols
id1 :: Dim1
id1 = id

-- | Convenience rewriting function for one dim2 symbol
id2 :: Dim2
id2 [c1,c2] = ([c1],[c2])
id2 _ = error "Only use id2 for single symbols! Write your own rewrite function instead."