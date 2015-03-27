{- | 
Space-efficient tabulation combinators.

Matrices are always filled in a triangular shape because a subword
with indices i and j fulfils i<=j. The triangular shape makes it
possible to use a space-efficient packed array for one-dimensional
nonterminals.

For two-dimensional nonterminals, the same logic is applied, although
there exists yet another source for space waste: overlapping subwords.
A better packing representation has yet to be found for this case.
-}
module ADP.Multi.TabulationTriangle where

import Data.Array
import ADP.Multi.Parser

-- | Two-dimensional tabulation for one-dim. parsers
--   using a packed one-dimensional array
table1' :: Array Int a -> Parser a b -> Parser a b
table1' z q = 
    let (_,n) = bounds z
        arr = array (0,(n+1)*(n+2) `div` 2)
                    [(adr (i,j), q z [i,j])
                    | i <- [0..n], j <- [i..n] ]
        adr (i,j) = i + (j*(j+1)) `div` 2
    in \ _ [i,j] -> 
        if i <= j
        then arr!adr (i,j)
        else error "invalid subword"

-- | Two-dimensional tabulation for one-dim. parsers
--   using a packed one-dimensional array
table1 :: Array Int a -> RichParser a b -> RichParser a b
table1 z (info,q) = (info, table1' z q)

-- | Four-dimensional tabulation for two-dim. parsers
--   using a packed two-dimensional array
table2' :: Array Int a -> Parser a b -> Parser a b
table2' z q =
    let (_,n) = bounds z
        arr = array ((0,0),((n+1)*(n+2) `div` 2,(n+1)*(n+2) `div` 2))
                    [ ((adr (i,j),adr (k,l)), q z [i,j,k,l])
                    | i <- [0..n], j <- [i..n]
                    , k <- [0..n], l <- [k..n] ]
        adr (i,j) = n*i - (i*(i-1)) `div` 2 + j
    in \ _ [i,j,k,l] -> 
        if i <= j && k <= l 
        then arr!(adr (i,j), adr (k,l))
        else error "invalid subword(s)" 

-- | Four-dimensional tabulation for two-dim. parsers
--   using a packed two-dimensional array
table2 :: Array Int a -> RichParser a b -> RichParser a b
table2 z (info,q) = (info, table2' z q)
