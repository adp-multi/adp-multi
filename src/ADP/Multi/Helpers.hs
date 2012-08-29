module ADP.Multi.Helpers where

import Data.Array
import ADP.Multi.Parser

-- TODO this is a workaround for now as we don't directly support 1-dim yet
-- for this to work, there has to be start rules like this:
{- s' (c1,c2) = ([],[c1,c2]) -- 1-dim simulated as 2-dim
   s = start <<< k >>> s' 
-}
axiom' :: Int -> Array Int a -> RichParser a b -> [b]
axiom' l z (_,ax) =  ax z [0,0,0,l]

        
-- # Create array from List

mk :: [a] -> Array Int a
mk xs = array (1,length xs) (zip [1..] xs)