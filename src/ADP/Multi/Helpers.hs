-- | Provides several convenience functions to ease parsing setup.
module ADP.Multi.Helpers (
    mk,
    mkTwoTrack,
    axiom,
    axiomTwoTrack
) where

import Control.Exception
import Data.Array
import ADP.Multi.Parser

-- | Turns an input sequence into an array for use with a 1-dim parser.
--   Typically, this prepares the input for the 'axiom' function.
mk :: [a] -> Array Int a
mk xs = array (1,length xs) (zip [1..] xs)

-- | Turns two input sequences into an array for use with a 2-dim parser.
--   Typically, this prepares the input for the 'axiomTwoTrack' function.
mkTwoTrack :: [a] -> [a] -> Array Int a
mkTwoTrack xs ys = mk (xs ++ ys)

-- | Convenience function for parsing a given input
--   using a 1-dim parser, usually the start nonterminal.
axiom :: Array Int a -> RichParser a b -> [b]
axiom z (_,ax) =
    let (_,l) = bounds z
    in ax z [0,l]
    
-- | Convenience function for parsing a given input pair
--   using a 2-dim parser, usually the start nonterminal.
axiomTwoTrack :: Eq a => Array Int a -> [a] -> [a] -> RichParser a b -> [b]
axiomTwoTrack z inp1 inp2 (_,ax) =
    assert (z == mkTwoTrack inp1 inp2) $
    ax z [0,l1,l1,l1+l2]
    where l1 = length inp1
          l2 = length inp2

