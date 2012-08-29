{-# LANGUAGE ImplicitParams #-}

module ADP.Multi.Combinators where

import Data.Maybe
import Data.Array
import qualified Control.Arrow as A

import ADP.Debug
import ADP.Multi.Parser
import ADP.Multi.Rewriting


{-

TODO

Weakening types:

To support dim1&2, we could put Parser1 and Parser2 into a data Parser and then
pattern match on that in the combinators. This won't cost performance as the pattern
matching is only done once. (wrong! see >>>|)
If more dimensions are implemented, then all combinators need to be adjusted with new cases.

=> In this branch, I tried that! Unfortunately, this led to nowhere. It complicates the code
   extremely and introduces lots of pattern matches in the parsing phase. The problem always was 
   to maintain type-safe Parser types which had either 2-tuple or 4-tuple as its accepted subword.
   But this leads to unmaintainable and monolithic code.  

An alternative to that would be to make the Subword in Parser generic as a list. Then
the subword in Ranges would also be a list instead of a tuple. The simple parser would
then pattern match his accepting subword as e.g. [x1,x2,x3,x4] and this would happen for
every call to a parser. It's not clear how well GHC optimizes this, probably not as much as tuples.
We would loose some type-safety and (probably) performance but still have readable code.


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
                result = case parser of
                  P1 parser' ->
                     \ [] -> P1 $ \ z subword -> map f (parser' z subword)
                  P2 parser' ->
                     \ [] -> P2 $ \ z subword -> map f (parser' z subword)
            in (
                 [info],
                 result                                  
            )

-- TODO parsers of different dim's should be mixable
-- this isn't really a problem for now as lower dimensions can be
-- simulated with higher dimensions by leaving some tuple elements of the rewriting rules empty
infixl 7 ~~~
(~~~) :: Parseable p a b => ([ParserInfo], [Ranges] -> Parser a (b -> c)) -> p -> ([ParserInfo], [Ranges] -> Parser a c)
(~~~) (infos,leftParser) parseable =
    let (info,rightParser) = toParser parseable
        newParser = case rightParser of
         P1 rightP ->
              \ ranges -> P1 $ \ z subword -> 
                      [ pr qr |
                        qr <- rightP z subword
                      , range <- ranges
                      , let lp = leftParser range
                      , let pr' = case lp of
                                     P1 lp' -> let Ranges1 rest sub = range in lp' z sub
                                     P2 lp' -> let Ranges2 rest sub = range in lp' z sub
                      , pr <- pr'
                      ]
    in (info : infos, newParser)
                             
     
-- special version of ~~~ which ignores the right parser for determining the yield sizes
-- this must be used for self-recursion
-- To make it complete, there should also be a special version of <<< but this isn't strictly
-- necessary as this can also be solved by not using left-recursion.
-- I guess this only works because of laziness (ignoring the info value of toParser).
infixl 7 ~~~|
(~~~|) :: Parseable p a b => ([ParserInfo], [Ranges] -> Parser a (b -> c)) -> p -> ([ParserInfo], [Ranges] -> Parser a c)
(~~~|) (infos,leftParser) parseable =
    let (_,rightParser) = toParser parseable
        info = ParserInfo2 { minYield=(0,0), maxYield=(Nothing,Nothing) }
        newParser = case (leftParser,rightParser) of
         (P1 leftP, P1 rightP) ->
              P1 $ \ ranges z subword -> 
                      [ pr qr |
                        qr <- rightParser z subword
                      , ~(Ranges1 sub rest) <- ranges
                      , pr <- leftParser rest z sub 
                      ]
         (P1 leftP, P2 rightP) ->
              P2 $ \ ranges z subword -> 
                      [ pr qr |
                        qr <- rightParser z subword
                      , ~(Ranges1 sub rest) <- ranges
                      , pr <- leftParser rest z sub 
                      ]
         (P2 leftP, P1 rightP) ->
              P1 $ \ ranges z subword -> 
                      [ pr qr |
                        qr <- rightParser z subword
                      , ~(Ranges2 sub rest) <- ranges
                      , pr <- leftParser rest z sub 
                      ]
         (P2 leftP, P2 rightP) ->
              P2 $ \ ranges z subword -> 
                      [ pr qr |
                        qr <- rightParser z subword
                      , ~(Ranges2 sub rest) <- ranges
                      , pr <- leftParser rest z sub 
                      ]
    in (info : infos, newParser)
           
-- TODO the dimension of the resulting parser should result from c
infix 6 >>>|
(>>>|) :: (?yieldAlg1 :: YieldAnalysisAlgorithm Dim1, ?rangeAlg1 :: RangeConstructionAlgorithm1)
       => ([ParserInfo], [Ranges] -> Parser a b) -> Dim1 -> RichParser a b
(>>>|) (infos,p) f =
        let yieldSize = ?yieldAlg1 f infos
        in trace (">>> yield size: " ++ show yieldSize) $
           case head infos of
              ParserInfo1 {} ->
               (yieldSize, P1 $
                  \ z subword ->
                    let ranges = ?rangeAlg1 f infos subword
                    in trace (">>> " ++ show subword) $
                       trace ("ranges: " ++ show ranges) $ 
                       [ result |
                         ~(Ranges1 sub rest) <- ranges
                       , let P1 p' = p rest -- TODO this pattern match might cost some performance
                       , result <- p' z sub 
                       ]
                )
                
infix 6 >>>||
(>>>||) :: (?yieldAlg2 :: YieldAnalysisAlgorithm Dim2, ?rangeAlg2 :: RangeConstructionAlgorithm2)
        => ([ParserInfo], [Ranges] -> Parser a b) -> Dim2 -> RichParser a b
(>>>||) (infos,p) f =
        let yieldSize = ?yieldAlg1 f infos
        in trace (">>> yield size: " ++ show yieldSize) $
           case head infos of
              ParserInfo1 {} ->
               (yieldSize, P1 $
                  \ z subword ->
                    let ranges = ?rangeAlg1 f infos subword
                    in trace (">>> " ++ show subword) $
                       trace ("ranges: " ++ show ranges) $ 
                       [ result |
                         ~(Ranges1 sub rest) <- ranges
                       , result <- p rest z sub 
                       ]
                )

infixr 5 ||| 
(|||) :: RichParser a b -> RichParser a b -> RichParser a b
(|||) (ParserInfo2 {minYield=minY1, maxYield=maxY1}, r) (ParserInfo2 {minYield=minY2, maxYield=maxY2}, q) = 
        (
              ParserInfo2 {
                 minYield = combineMinYields minY1 minY2,
                 maxYield = combineMaxYields maxY1 maxY2
              },
              \ z (i,j,k,l) -> r z (i,j,k,l) ++ q z (i,j,k,l)
        )        

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


type Filter2 a = Array Int a -> Subword2 -> Bool
with2 :: RichParser a b -> Filter2 a -> RichParser a b
with2 (info,P2 q) c =
        (
            info,
            P2 $ \ z subword -> if c z subword then q z subword else []
        )
