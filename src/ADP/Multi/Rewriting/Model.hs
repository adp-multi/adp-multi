-- | Default model of rewriting functions used in adp-multi.
module ADP.Multi.Rewriting.Model where

{- |
Every 1-dim parser has one symbol, every 2-dim parser two symbols.
In a production with parsers p1 to pn, each parser has a number,
1 to n. Each symbol of a parser also has a number, 1 or 2, as only
two dimensions are supported now. Both numbers form a unique identifier
for each symbol in a production.

Example:
f <<< a ~~~ b ~~~ c >>> r

a and c shall have dimension 1, b dimension 2.
Then a has id (1,1), b has ids (2,1) and (2,2), and
c has (3,1). Applying a rewriting function of type 'Dim1' or 'Dim2'
to the list of ids produces a permutation of those, possibly
split up in two dimensions.

E.g., [(1,1),(2,1),(2,2),(3,1)] gets ([(2,1),(3,1)],[(2,2),(1,1)])
if the rewriting function is: r [a,b1,b2,c] = ([b1,c],[b2,a]).
-} 
type SymbolID = (Int, Int)

-- | 1-dimensional rewriting function
type Dim1 = [SymbolID] -> [SymbolID] 

-- | 2-dimensional rewriting function
--   Note: every dimension must contain at least one element
type Dim2 = [SymbolID] -> ([SymbolID], [SymbolID])

-- | Convenience rewriting function for one or more dim1 parsers
id1 :: Dim1
id1 = id

-- | Convenience rewriting function for one dim2 parser
id2 :: Dim2
id2 [c1,c2] = ([c1],[c2])
id2 _ = error "Only use id2 for single parsers! Write your own rewriting function instead."