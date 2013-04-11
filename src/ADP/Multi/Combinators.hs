{-# LANGUAGE MultiParamTypeClasses #-}

-- | Parser combinators for use in grammars
module ADP.Multi.Combinators (
    (<<<),
    (~~~),
    Rewritable(..), rewrite,
    (|||),
    (...),
    Filter, with,
    yieldSize1, yieldSize2
) where

import Data.Array
import Control.Monad (liftM2)

import ADP.Multi.Parser
import ADP.Multi.Rewriting



eval :: (b -> c) -> Parser a b -> ([SubwordTree] -> Parser a c)
eval f parser [] z subword = map f (parser z subword) 

infix 8 <<<
(<<<) :: Parseable p a b 
      => (b -> c)
      -> p
      -> ([ParserInfo], [SubwordTree] -> Parser a c)
(<<<) f parseable =
    let (info,parser) = toParser parseable
    in ([info], eval f parser)

seqDefer :: ([SubwordTree] -> Parser a (b -> c)) 
         -> Parser a b
         -> ([SubwordTree] -> Parser a c)
seqDefer leftParser rightParser subwordTrees z subword =
      [ pr qr |
        qr <- rightParser z subword
      , SubwordTree sub rest <- subwordTrees
      , pr <- leftParser rest z sub 
      ]

infixl 7 ~~~
(~~~) :: Parseable p a b 
      => ([ParserInfo], [SubwordTree] -> Parser a (b -> c))
      -> p
      -> ([ParserInfo], [SubwordTree] -> Parser a c)
(~~~) (infos,leftParser) parseable =
        let (info,rightParser) = toParser parseable
        in (info : infos, seqDefer leftParser rightParser)  
                
-- | Explicitly specify yield size of a parser.
yieldSize :: ParserInfo -> RichParser a b -> RichParser a b
yieldSize info (_,p) = (info, p)

-- two convenience functions so that ParserInfo stays hidden

-- | Explicitly specify yield size of a 1-dim parser.
yieldSize1 :: (Int,Maybe Int) -> RichParser a b -> RichParser a b
yieldSize1 (minY,maxY) = 
    yieldSize (ParserInfo1 minY maxY)

-- | Explicitly specify yield size of a 2-dim parser.
yieldSize2 :: (Int,Maybe Int) -> (Int,Maybe Int) 
           -> RichParser a b -> RichParser a b
yieldSize2 (minY1,maxY1) (minY2,maxY2) = 
    yieldSize (ParserInfo2 (minY1,minY2) (maxY1,maxY2))


rewrite :: SubwordConstructionAlgorithm a
        -> ([ParserInfo], [SubwordTree] -> Parser b c) 
        -> a            -- ^ rewriting function
        -> Parser b c
rewrite subwordAlg (infos,p) r z subword =
    let subwordTrees = subwordAlg r infos subword
    in [ result |
         SubwordTree sub rest <- subwordTrees
       , result <- p rest z sub
       ]
       
class Rewritable r a b where
    infix 6 >>>
    (>>>) :: ([ParserInfo], [SubwordTree] -> Parser a b) -> r -> RichParser a b          

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


{- |
Filters are not part of ADP-MCFL, but are sometimes used in RNA folding
to skip parses where subwords are too long, e.g. restricting loop size
to 30. It is included here for convenience.
-} 
type Filter a = Array Int a -> Subword -> Bool

with' :: Parser a b -> Filter a -> Parser a b
with' q c z subword = if c z subword then q z subword else []

with :: RichParser a b -> Filter a -> RichParser a b
with (info,q) c = (info, with' q c)