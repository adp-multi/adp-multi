module ADP.Multi.Combinators where

import Debug.HTrace (htrace)
import Data.Maybe
import Data.Array
import qualified Control.Arrow as A

import ADP.Multi.Parser
import ADP.Multi.Rewriting

trace _ b = b
--trace = htrace


infix 8 <<<
(<<<) :: Parseable p a b => (b -> c) -> p -> ([ParserInfo2], [Ranges] -> Parser2 a c)
(<<<) f parseable =
            let (info,parser) = toParser parseable
            in (
                 [info],
                 \ [] z subword -> map f (parser z subword)                                  
            )

-- TODO parsers of different dim's should be mixable
-- this isn't really a problem for now as lower dimensions can be
-- simulated with higher dimensions by leaving some tuple elements of the rewriting rules empty
infixl 7 ~~~
(~~~) :: Parseable p a b => ([ParserInfo2], [Ranges] -> Parser2 a (b -> c)) -> p -> ([ParserInfo2], [Ranges] -> Parser2 a c)
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
-- this must be used for self-recursion
-- To make it complete, there should also be a special version of <<< but this isn't strictly
-- necessary as this can also be solved by not using left-recursion.
-- I guess this only works because of laziness (ignoring the info value of toParser).
infixl 7 ~~~|
(~~~|) :: Parseable p a b => ([ParserInfo2], [Ranges] -> Parser2 a (b -> c)) -> p -> ([ParserInfo2], [Ranges] -> Parser2 a c)
(~~~|) (infos,leftParser) parseable =
        let (_,rightParser) = toParser parseable
            info = ParserInfo2 { minYield=(0,0), maxYield=(Nothing,Nothing) }
        in (
                info : infos,
                \ ranges z subword -> 
                        [ pr qr |
                          qr <- rightParser z subword
                        , RangeMap sub rest <- ranges
                        , pr <- leftParser rest z sub 
                        ]
           )
           
-- TODO the dimension of the resulting parser should result from c
infix 6 >>>
(>>>) :: Rewriting c => ([ParserInfo2], [Ranges] -> Parser2 a b) -> c -> RichParser2 a b
(>>>) (infos,p) f =
        let yieldSize = determineYieldSize f infos
        in trace (">>> yield size: " ++ show yieldSize) $
           (
              yieldSize,
              \ z subword ->
                let ranges = constructRanges f infos subword
                in trace (">>> " ++ show subword) $
                trace ("ranges: " ++ show ranges) $ 
                [ result |
                  RangeMap sub rest <- ranges
                , result <- p rest z sub 
                ]
           )

infixr 5 ||| 
(|||) :: RichParser2 a b -> RichParser2 a b -> RichParser2 a b
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
(...) :: RichParser2 a b -> ([b] -> [b]) -> RichParser2 a b
(...) (info,r) h = (info, \ z subword -> h (r z subword) )
--(...) richParser h = A.second (\ r z subword -> h (r z subword) ) richParser


type Filter2 a = Array Int a -> Subword2 -> Bool
with2 :: Parser2 a b -> Filter2 a -> Parser2 a b
with2 q c z subword = if c z subword then q z subword else []
