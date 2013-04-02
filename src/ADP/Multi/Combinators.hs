module ADP.Multi.Combinators (
    (<<<),(<<<|),(<<<||),
    (~~~),(~~~|),(~~~||),
    (>>>|),(>>>||),
    (|||),
    (...),
    with
) where

import Data.Maybe
import Data.Array
import qualified Control.Arrow as A

import ADP.Debug
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



-- TODO use static info about min yield sizes for self-recursion
--      This is not easy to solve for indirect recursion like S -> a | aP, P -> aS | a
--      At the moment we would use S -> a | a ~~~| P and P -> a ~~~| S | a to prevent
--      endless recursion at yield size analysis. Therefore, as ~~~| isn't only used
--      for direct self-recursion, we would need to analyse the grammar in its whole to
--      detect cycles which seems impossible without creating a complete AST.
-- TODO define which grammars are not useable without a whole-grammar yield size analysis



infix 8 <<<
(<<<) :: Parseable p a b => (b -> c) -> p -> ([ParserInfo], [Ranges] -> Parser a c)
(<<<) f parseable =
            let (info,parser) = toParser parseable
            in (
                 [info],
                 \ [] z subword -> map f (parser z subword)                                  
            )

-- special version of <<< which ignores the first parser for determining the yield sizes
-- for dim1 parsers            
infix 8 <<<|
(<<<|) :: Parseable p a b => (b -> c) -> p -> ([ParserInfo], [Ranges] -> Parser a c)
(<<<|) f parseable =
            let (_,parser) = toParser parseable
                info = ParserInfo1 { minYield = 0, maxYield = Nothing }
            in (
                 [info],
                 \ [] z subword -> map f (parser z subword)                                  
            )

-- special version of <<< which ignores the first parser for determining the yield sizes
-- for dim2 parsers                
infix 8 <<<||
(<<<||) :: Parseable p a b => (b -> c) -> p -> ([ParserInfo], [Ranges] -> Parser a c)
(<<<||) f parseable =
            let (_,parser) = toParser parseable
                info = ParserInfo2 { minYield2 = (0,0), maxYield2 = (Nothing,Nothing) }
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
                             
     
-- special version of ~~~ which ignores the right parser for determining the yield sizes
-- this must be used for self-recursion, mutual recursion etc. There must be no cycles! 
-- I guess this only works because of laziness (ignoring the info value of toParser).
-- for 1-dim parsers 
infixl 7 ~~~|
(~~~|) :: Parseable p a b => ([ParserInfo], [Ranges] -> Parser a (b -> c)) -> p -> ([ParserInfo], [Ranges] -> Parser a c)
(~~~|) (infos,leftParser) parseable =
        let (_,rightParser) = toParser parseable
            info = ParserInfo1 { minYield = 0, maxYield = Nothing }
        in (
                info : infos,
                \ ranges z subword -> 
                        [ pr qr |
                          qr <- rightParser z subword
                        , RangeMap sub rest <- ranges
                        , pr <- leftParser rest z sub 
                        ]
           )

-- for 2-dim parsers
infixl 7 ~~~||
(~~~||) :: Parseable p a b => ([ParserInfo], [Ranges] -> Parser a (b -> c)) -> p -> ([ParserInfo], [Ranges] -> Parser a c)
(~~~||) (infos,leftParser) parseable =
        let (_,rightParser) = toParser parseable
            info = ParserInfo2 { minYield2 = (0,0), maxYield2 = (Nothing,Nothing) }
        in (
                info : infos,
                \ ranges z subword -> 
                        [ pr qr |
                          qr <- rightParser z subword
                        , RangeMap sub rest <- ranges
                        , pr <- leftParser rest z sub 
                        ]
           )
           
infix 6 >>>|
(>>>|) :: ([ParserInfo], [Ranges] -> Parser a b) -> Dim1 -> RichParser a b
(>>>|) = rewrite determineYieldSize1 constructRanges1

infix 6 >>>||
(>>>||) :: ([ParserInfo], [Ranges] -> Parser a b) -> Dim2 -> RichParser a b
(>>>||) = rewrite determineYieldSize2 constructRanges2
           
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
--(...) richParser h = A.second (\ r z subword -> h (r z subword) ) richParser


type Filter a = Array Int a -> Subword -> Bool
with :: RichParser a b -> Filter a -> RichParser a b
with (info,q) c =
        (
            info,
            \ z subword -> if c z subword then q z subword else []
        )
