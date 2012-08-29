{-# LANGUAGE PatternGuards #-}

module ADP.Multi.Helpers where

import Data.Array
import ADP.Multi.Parser

axiom1 :: Array Int a -> RichParser a b -> [b]
axiom1 z richParser
  | RP1 (_,ax) <- richParser = ax z (0,l)
  | otherwise = error "dimension of start must be 1"
  where (_,l) = bounds z

        
-- # Create array from List

mk :: [a] -> Array Int a
mk xs = array (1,length xs) (zip [1..] xs)