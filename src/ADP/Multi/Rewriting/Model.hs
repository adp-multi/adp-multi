-- | Default model of rewriting functions used in adp-multi.
module ADP.Multi.Rewriting.Model where

type Dim1 = [(Int, Int)] -> [(Int, Int)] 
type Dim2 = [(Int, Int)] -> ([(Int, Int)], [(Int, Int)])

-- | Convenience rewriting function for one or more dim1 symbols
id1 :: Dim1
id1 = id

-- | Convenience rewriting function for one dim2 symbol
id2 :: Dim2
id2 [c1,c2] = ([c1],[c2])
id2 _ = error "Only use id2 for single symbols! Write your own rewrite function instead."