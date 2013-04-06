{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module ADP.Multi.Combinators (
    (<<<),
    (~~~),
    yieldSize1, yieldSize2,
    (>>>),
    (|||),
    (...),
    with,
    rewrite -- for CombinatorsConstraint
) where

import Data.Maybe
import Data.Array
import Control.Monad (liftM2)

import ADP.Multi.Parser
import ADP.Multi.Rewriting
import ADP.Multi.Rewriting.YieldSize
import ADP.Multi.Rewriting.Explicit

{-

TODO

Weakening types:

The Subword in Parser could be made generic as a list. Then
the subword in Ranges would also be a list instead of a tuple. The simple parser would
then pattern match his accepting subword as e.g. [x1,x2,x3,x4] and this would happen for
every call to a parser. It's not clear how well GHC optimizes this, probably not as much as tuples.
We would loose some type-safety and (probably) performance but still have readable code.

=> This branch tries the above approach.

Using full type system without data-constructs:

If no data-constructs are used, then instead we need many combinations of overloads simulated with
type classes. Then the rewriting functions would also (need to) be type-safe, but it is not yet clear
how to do that.


-> Considering that this is a prototype and probably won't be used in this form, it might be too much effort
to get full type-safety.
 

-}

eval :: (b -> c) -> Parser a b -> ([Ranges] -> Parser a c)
eval f parser [] z subword = map f (parser z subword) 

infix 8 <<<
(<<<) :: Parseable p a b 
      => (b -> c)
      -> p
      -> ([ParserInfo], [Ranges] -> Parser a c)
(<<<) f parseable =
    let (info,parser) = toParser parseable
    in ([info], eval f parser)

seqDefer :: ([Ranges] -> Parser a (b -> c)) 
         -> Parser a b
         -> ([Ranges] -> Parser a c)
seqDefer leftParser rightParser ranges z subword =
      [ pr qr |
        qr <- rightParser z subword
      , RangeMap sub rest <- ranges
      , pr <- leftParser rest z sub 
      ]

infixl 7 ~~~
(~~~) :: Parseable p a b 
      => ([ParserInfo], [Ranges] -> Parser a (b -> c))
      -> p
      -> ([ParserInfo], [Ranges] -> Parser a c)
(~~~) (infos,leftParser) parseable =
        let (info,rightParser) = toParser parseable
        in (info : infos, seqDefer leftParser rightParser)  
                
-- | explicitly specify yield size of a parser
-- | needed to break dependency cycles between nonterminals
yieldSize :: ParserInfo -> RichParser a b -> RichParser a b
yieldSize info (_,p) = (info, p)

-- two convenience functions so that ParserInfo stays hidden
yieldSize1 :: (Int,Maybe Int) -> RichParser a b -> RichParser a b
yieldSize1 (minY,maxY) = 
    yieldSize (ParserInfo1 minY maxY)

yieldSize2 :: (Int,Maybe Int) -> (Int,Maybe Int) 
           -> RichParser a b -> RichParser a b
yieldSize2 (minY1,maxY1) (minY2,maxY2) = 
    yieldSize (ParserInfo2 (minY1,minY2) (maxY1,maxY2))


rewrite :: RangeConstructionAlgorithm a
        -> ([ParserInfo], [Ranges] -> Parser b c) 
        -> a    -- rewriting function, Dim1 or Dim2
        -> Parser b c
rewrite rangeAlg (infos,p) f z subword =
    let ranges = rangeAlg f infos subword
    in [ result |
         RangeMap sub rest <- ranges
       , result <- p rest z sub 
       ]
       
class Rewritable r a b where
    infix 6 >>>
    (>>>) :: ([ParserInfo], [Ranges] -> Parser a b) -> r -> RichParser a b

instance Rewritable Dim1 a b where
    (>>>) (infos,p) f = 
      (determineYieldSize1 f infos, rewrite constructRanges1 (infos,p) f)
    
instance Rewritable Dim2 a b where
    (>>>) (infos,p) f = 
      (determineYieldSize2 f infos, rewrite constructRanges2 (infos,p) f)
          

alt :: Parser a b -> Parser a b -> Parser a b
alt r q z subword = r z subword ++ q z subword 

infixr 5 ||| 
(|||) :: RichParser a b -> RichParser a b -> RichParser a b
(|||) (ParserInfo1 minY1 maxY1, r) (ParserInfo1 minY2 maxY2, q) = 
        (
              ParserInfo1 {
                 minYield = min minY1 minY2,
                 maxYield = liftM2 max maxY1 maxY2
              },
              alt r q
        )    
(|||) (ParserInfo2 (minY11,minY12) (maxY11,maxY12), r) 
      (ParserInfo2 (minY21,minY22) (maxY21,maxY22), q) = 
        (
              ParserInfo2 {
                 minYield2 = (min minY11 minY21, min minY12 minY22),
                 maxYield2 = (liftM2 max maxY11 maxY21, liftM2 max maxY12 maxY22)                 
              },
              alt r q
        )
(|||) _ _ = error "Different parser dimensions can't be combined with ||| !"


select :: Parser a b -> ([b] -> [b]) -> Parser a b
select r h z subword = h (r z subword)

infix  4 ...
(...) :: RichParser a b -> ([b] -> [b]) -> RichParser a b
(...) (info,r) h = (info, select r h)


{- Filters are not part of ADP-MCFL, but are sometimes used in RNA folding
   to skip parses where subwords are too long, e.g. restricting loop size
   to 30. It is included here for convenience.
-} 
type Filter a = Array Int a -> Subword -> Bool

with' :: Parser a b -> Filter a -> Parser a b
with' q c z subword = if c z subword then q z subword else []

with :: RichParser a b -> Filter a -> RichParser a b
with (info,q) c = (info, with' q c)