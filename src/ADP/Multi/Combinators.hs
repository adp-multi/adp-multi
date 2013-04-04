{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module ADP.Multi.Combinators (
    (<<<),
    (~~~),
    yieldSize1, yieldSize2,
    (>>>),
    (|||),
    (...),
    with
) where

import Data.Maybe
import Data.Array

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



infix 8 <<<
(<<<) :: Parseable p a b => (b -> c) -> p -> ([ParserInfo], [Ranges] -> Parser a c)
(<<<) f parseable =
            let (info,parser) = toParser parseable
            in (
                 [info],
                 \ [] z subword -> map f (parser z subword)                                  
            )

infixl 7 ~~~
(~~~) :: Parseable p a b => ([ParserInfo], [Ranges] -> Parser a (b -> c)) -> p -> ([ParserInfo], [Ranges] -> Parser a c)
(~~~) (infos,leftParser) parseable =
        let (info,rightParser) = toParser parseable
        in (
                info : infos,
                \ ranges z subword -> 
                      [ pr qr |
                        qr <- rightParser z subword
                      , RangeMap sub rest <- ranges
                      , pr <- leftParser rest z sub 
                      ]
           )
                             
                
-- | explicitily specify yield size of a parser
-- | needed to break dependency cycles between nonterminals
yieldSize :: ParserInfo -> RichParser a b -> RichParser a b
yieldSize info (_,p) = (info, p)

-- two convenience functions so that ParserInfo stays hidden
yieldSize1 :: (Int,Maybe Int) -> RichParser a b -> RichParser a b
yieldSize1 (minY,maxY) = 
    yieldSize ParserInfo1 {minYield=minY, maxYield=maxY}

yieldSize2 :: (Int,Maybe Int) -> (Int,Maybe Int) -> RichParser a b -> RichParser a b
yieldSize2 (minY1,maxY1) (minY2,maxY2) = 
    yieldSize ParserInfo2 {minYield2=(minY1,minY2), maxYield2=(maxY1,maxY2)}

-- some syntax sugar to prevent having >>>| and >>>|| or similar
class Rewritable r a b where
    infix 6 >>>
    (>>>) :: ([ParserInfo], [Ranges] -> Parser a b) -> r -> RichParser a b

instance Rewritable Dim1 a b where
    (>>>) = rewrite determineYieldSize1 constructRanges1
    
instance Rewritable Dim2 a b where
    (>>>) = rewrite determineYieldSize2 constructRanges2
          
           
infixr 5 ||| 
(|||) :: RichParser a b -> RichParser a b -> RichParser a b
(|||) (ParserInfo1 {minYield=minY1, maxYield=maxY1}, r) (ParserInfo1 {minYield=minY2, maxYield=maxY2}, q) = 
        (
              ParserInfo1 {
                 minYield = min minY1 minY2,
                 maxYield = if isNothing maxY1 || isNothing maxY2 then Nothing else max maxY1 maxY2
              },
              \ z subword -> r z subword ++ q z subword
        )    
(|||) (ParserInfo2 {minYield2=minY1, maxYield2=maxY1}, r) (ParserInfo2 {minYield2=minY2, maxYield2=maxY2}, q) = 
        (
              ParserInfo2 {
                 minYield2 = combineMinYields minY1 minY2,
                 maxYield2 = combineMaxYields maxY1 maxY2
              },
              \ z subword -> r z subword ++ q z subword
        )
(|||) _ _ = error "Different parser dimensions can't be combined with ||| !"

combineMinYields :: (Int,Int) -> (Int,Int) -> (Int,Int)
combineMinYields (min11,min12) (min21,min22) = (min min11 min21, min min12 min22)

combineMaxYields :: (Maybe Int,Maybe Int) -> (Maybe Int,Maybe Int) -> (Maybe Int,Maybe Int)
combineMaxYields (a,b) (c,d) =
        ( if isNothing a || isNothing c then Nothing else max a c
        , if isNothing b || isNothing d then Nothing else max b d
        )

infix  4 ...
(...) :: RichParser a b -> ([b] -> [b]) -> RichParser a b
(...) (info,r) h = (info, \ z subword -> h (r z subword) )


type Filter a = Array Int a -> Subword -> Bool
with :: RichParser a b -> Filter a -> RichParser a b
with (info,q) c =
        (
            info,
            \ z subword -> if c z subword then q z subword else []
        )
