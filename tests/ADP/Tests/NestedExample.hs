{-# LANGUAGE ImplicitParams #-}

module ADP.Tests.NestedExample where

import ADP.Multi.SimpleParsers
import ADP.Multi.Combinators
import ADP.Multi.Tabulation
import ADP.Multi.Helpers
import ADP.Multi.Rewriting
                                 
type Nested_Algebra alphabet answer = (
  EPS -> answer,                              -- nil
  answer   -> answer -> answer,               -- left
  answer   -> answer -> answer,               -- pair
  alphabet -> answer -> alphabet -> answer,   -- basepair
  alphabet -> answer,                         -- base
  [answer] -> [answer]                        -- h
  )

-- test using record syntax
data NestedAlgebra alphabet answer = NestedAlgebra {
  nil :: EPS -> answer,          
  left :: answer -> answer -> answer,
  pair :: answer -> answer -> answer,
  basepair :: alphabet -> answer -> alphabet -> answer,
  base :: alphabet -> answer,
  h :: [answer] -> [answer]
  }
  
infixl ***
(***) :: (Eq b, Eq c) => Nested_Algebra a b -> Nested_Algebra a c -> Nested_Algebra a (b,c)
alg1 *** alg2 = (nil,left,pair,basepair,base,h) where
   (nil',left',pair',basepair',base',h') = alg1
   (nil'',left'',pair'',basepair'',base'',h'') = alg2
   
   nil a = (nil' a, nil'' a)
   left (b1,b2) (s1,s2) = (left' b1 s1, left'' b2 s2)
   pair (p1,p2) (s1,s2) = (pair' p1 s1, pair'' p2 s2)
   basepair a (s1,s2) b = (basepair' a s1 b,  basepair'' a s2 b)
   base a = (base' a, base'' a)
   h xs = [ (x1,x2) |
            x1 <- h'  [ y1 | (y1,_)  <- xs]
          , x2 <- h'' [ y2 | (y1,y2) <- xs, y1 == x1]
          ]


data Start = Nil
           | Left' Start Start
           | Pair Start Start
           | BasePair Char Start Char
           | Base Char
           deriving (Eq, Show)

-- without consistency checks
enum :: Nested_Algebra Char Start
enum = (nil,left,pair,basepair,base,h) where
   nil _     = Nil
   left      = Left'
   pair      = Pair 
   basepair  = BasePair
   base      = Base
   h         = id 
   
enum' :: NestedAlgebra Char Start
enum' = NestedAlgebra {
   nil       = \ _ -> Nil, -- hmm, this sucks
   left      = Left',
   pair      = Pair,
   basepair  = BasePair,
   base      = Base,
   h         = id
   }
   
maxBasepairs :: Nested_Algebra Char Int
maxBasepairs = (nil,left,pair,basepair,base,h) where
   nil _            = 0
   left a b         = a + b
   pair a b         = a + b
   basepair _ s _   = 1 + s
   base _           = 0
   h []             = []
   h xs             = [maximum xs]

-- The left part is the structure and the right part the reconstructed input.
prettyprint :: Nested_Algebra Char (String,String)
prettyprint = (nil,left,pair,basepair,base,h) where
   nil _ = ("","")
   left (bl,br) (sl,sr) = (bl ++ sl, br ++ sr)
   pair (pl,pr) (sl,sr) = (pl ++ sl, pr ++ sr)
   basepair b1 (sl,sr) b2 = ("(" ++ sl ++ ")", [b1] ++ sr ++ [b2])
   base b = (".", [b])
   h = id
   
pstree :: Nested_Algebra Char String
pstree = (nil,left,pair,basepair,base,h) where
   nil _ = "\\emptyword"
   left b s = nonterm "B" b ++ nonterm "S" s
   pair p s = nonterm "P" p ++ nonterm "S" s
   basepair b1 s b2 = base b1 ++ nonterm "S" s ++ base b2
   base b = "\\terminal{" ++ [b] ++ "}"
   h = id
   
   nonterm sym tree = "\\pstree{\\nonterminal{" ++ sym ++ "}}{" ++ tree ++ "}"
   
term :: Nested_Algebra Char String
term = (nil,left,pair,basepair,base,h) where
   nil _ = "\\op{f}_3()"
   left b s = "\\op{f}_2(" ++ b ++ "," ++ s ++ ")"
   pair p s = "\\op{f}_2(" ++ p ++ "," ++ s ++ ")"
   basepair b1 s b2 = "\\op{f}_4(" ++ [b1] ++ "," ++ s ++ "," ++ [b2] ++ ")"
   base b = "\\op{f}_5(" ++ [b] ++ ")"
   h = id
   
nested :: YieldAnalysisAlgorithm Dim1 -> RangeConstructionAlgorithm Dim1
       -> Nested_Algebra Char answer -> String -> [answer]
nested yieldAlg1 rangeAlg1 algebra inp =
  -- These implicit parameters are used by >>>.
  -- They were introduced to allow for exchanging the algorithms and
  -- they were made implicit so that they don't ruin our nice syntax.
  let ?yieldAlg1 = yieldAlg1
      ?rangeAlg1 = rangeAlg1
  in let
  
  (nil,left,pair,basepair,base,h) = algebra
     
  s = tabulated $
      nil  <<< EPS >>>| id |||
      left <<< b ~~~| s >>>| id |||
      pair <<< p ~~~| s >>>| id
      ... h
  
  b = tabulated $
      base <<< 'a' >>>| id |||
      base <<< 'u' >>>| id |||
      base <<< 'c' >>>| id |||
      base <<< 'g' >>>| id
  
  p = tabulated $
      basepair <<< 'a' ~~~| s ~~~ 'u' >>>| id |||
      basepair <<< 'u' ~~~| s ~~~ 'a' >>>| id |||
      basepair <<< 'c' ~~~| s ~~~ 'g' >>>| id |||
      basepair <<< 'g' ~~~| s ~~~ 'c' >>>| id |||
      basepair <<< 'g' ~~~| s ~~~ 'u' >>>| id |||
      basepair <<< 'u' ~~~| s ~~~ 'g' >>>| id
      
  z = mk inp
  tabulated = table1 z
  
  in axiom z s