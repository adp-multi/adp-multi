{-# LANGUAGE ImplicitParams #-}

{- This example implements the 1-structure grammar from
   "Topology and prediction of RNA pseudoknots" by Reidys et al., 2011
-}
module ADP.Tests.OneStructureExample where

import Data.Array

import ADP.Multi.Parser
import ADP.Multi.SimpleParsers
import ADP.Multi.Combinators
import ADP.Multi.Tabulation
import ADP.Multi.Helpers
import ADP.Multi.Rewriting
                                 
type OneStructure_Algebra alphabet answer = (
  EPS -> answer,                              -- nil
  answer -> answer -> answer,               -- left
  answer -> answer -> answer -> answer,     -- pair
  (alphabet, alphabet) -> answer,             -- basepair
  alphabet -> answer,                         -- base
  answer -> answer,                           -- i1
  answer -> answer,                           -- i2
  answer -> answer -> answer -> answer -> answer, -- tstart
  answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer, -- knotH
  answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer, -- knotK
  answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer, -- knotL
  answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer, -- knotM
  answer -> answer -> answer -> answer -> answer, -- aknot1
  answer -> answer,                               -- aknot2
  answer -> answer -> answer -> answer -> answer, -- bknot1
  answer -> answer,                               -- bknot2
  answer -> answer -> answer -> answer -> answer, -- cknot1
  answer -> answer,                               -- cknot2
  answer -> answer -> answer -> answer -> answer, -- dknot1
  answer -> answer,                               -- dknot2
  [answer] -> [answer]                        -- h
  )
  
data T = Nil
       | Left' T T
       | Pair T T T
       | BasePair (Char, Char)
       | Base Char
       | I1 T
       | I2 T
       | TStart T T T T
       | KnotH T T T T T T T
       | KnotK T T T T T T T T T T
       | KnotL T T T T T T T T T T
       | KnotM T T T T T T T T T T T T T
       | XKnot1 T T T T
       | XKnot2 T
       deriving (Eq, Show)

enum :: OneStructure_Algebra Char T
enum = (\_->Nil,Left',Pair,BasePair,Base,I1,I2,TStart,KnotH,KnotK,KnotL,KnotM
       ,XKnot1,XKnot2,XKnot1,XKnot2,XKnot1,XKnot2,XKnot1,XKnot2,id)
   
prettyprint :: OneStructure_Algebra Char [String]
prettyprint = (nil,left,pair,basepair,base,i1,i2,tstart,knotH,knotK,knotL,knotM
              ,aknot1,aknot2,bknot1,bknot2,cknot1,cknot2,dknot1,dknot2,h) where
   nil _ = [""]
   left b s = [concat $ b ++ s]
   pair [p1,p2] s1 s2 = [concat $ [p1] ++ s1 ++ [p2] ++ s2]
   basepair _ = ["(",")"]
   base _ = ["."]
   i1 s = s
   i2 t = t
   tstart [p1,p2] i t s = [concat $ i ++ [p1] ++ t ++ [p2] ++ s]
   knotH s i1 i2 i3 i4 [a1,a2] [b1,b2] =
      [concat $ i1 ++ [a1] ++ i2 ++ [b1] ++ i3 ++ [a2] ++ i4 ++ [b2] ++ s]
   knotK s i1 i2 i3 i4 i5 i6 [a1,a2] [b1,b2] [c1,c2] =
      [concat $ i1 ++ [a1] ++ i2 ++ [b1] ++ i3 ++ [a2] ++ i4 ++ [c1] ++ i5 ++ [b2] ++ i6 ++ [c2] ++ s]
   knotL s i1 i2 i3 i4 i5 i6 [a1,a2] [b1,b2] [c1,c2] =
      [concat $ i1 ++ [a1] ++ i2 ++ [b1] ++ i3 ++ [c1] ++ i4 ++ [a2] ++ i5 ++ [b2] ++ i6 ++ [c2] ++ s]
   knotM s i1 i2 i3 i4 i5 i6 i7 i8 [a1,a2] [b1,b2] [c1,c2] [d1,d2] =
      [concat $ i1 ++ [a1] ++ i2 ++ [b1] ++ i3 ++ [c1] ++ i4 ++ [a2] ++ i5 ++ [d1] ++ i6 ++ [b2] ++ i7 ++ [c2] ++ i8 ++ [d2] ++ s]
   aknot1 _ = xknot1 "(" ")"
   aknot2 _ = [ "(" , ")" ]
   bknot1 _ = xknot1 "[" "]"
   bknot2 _ = [ "{" , "}" ]
   cknot1 _ = xknot1 "{" "}"
   cknot2 _ = [ "{" , "}" ]
   dknot1 _ = xknot1 "<" ">"
   dknot2 _ = [ "<" , ">" ]
   
   xknot1 parenL parenR i1 i2 [x1,x2] = [concat $ [parenL] ++ i1 ++ [x1], concat $ [x2] ++ i2 ++ [parenR]]
      
   h = id
   
-- reconstructed input
prettyprint2 :: OneStructure_Algebra Char [String]
prettyprint2 = (nil,left,pair,basepair,base,i1,i2,tstart,knotH,knotK,knotL,knotM
              ,aknot1,aknot2,bknot1,bknot2,cknot1,cknot2,dknot1,dknot2,h) where
   nil _ = [""]
   left b s = [concat $ b ++ s]
   pair [p1,p2] s1 s2 = [concat $ [p1] ++ s1 ++ [p2] ++ s2]
   basepair (b1,b2) = [[b1],[b2]]
   base b = [[b]]
   i1 s = s
   i2 t = t
   tstart [p1,p2] i t s = [concat $ i ++ [p1] ++ t ++ [p2] ++ s]
   knotH s i1 i2 i3 i4 [a1,a2] [b1,b2] =
      [concat $ i1 ++ [a1] ++ i2 ++ [b1] ++ i3 ++ [a2] ++ i4 ++ [b2] ++ s]
   knotK s i1 i2 i3 i4 i5 i6 [a1,a2] [b1,b2] [c1,c2] =
      [concat $ i1 ++ [a1] ++ i2 ++ [b1] ++ i3 ++ [a2] ++ i4 ++ [c1] ++ i5 ++ [b2] ++ i6 ++ [c2] ++ s]
   knotL s i1 i2 i3 i4 i5 i6 [a1,a2] [b1,b2] [c1,c2] =
      [concat $ i1 ++ [a1] ++ i2 ++ [b1] ++ i3 ++ [c1] ++ i4 ++ [a2] ++ i5 ++ [b2] ++ i6 ++ [c2] ++ s]
   knotM s i1 i2 i3 i4 i5 i6 i7 i8 [a1,a2] [b1,b2] [c1,c2] [d1,d2] =
      [concat $ i1 ++ [a1] ++ i2 ++ [b1] ++ i3 ++ [c1] ++ i4 ++ [a2] ++ i5 ++ [d1] ++ i6 ++ [b2] ++ i7 ++ [c2] ++ i8 ++ [d2] ++ s]
   aknot1 = xknot1
   aknot2 = xknot2
   bknot1 = xknot1
   bknot2 = xknot2
   cknot1 = xknot1
   cknot2 = xknot2
   dknot1 = xknot1
   dknot2 = xknot2
   
   xknot1 [p1,p2] i1 i2 [x1,x2] = [concat $ [p1] ++ i1 ++ [x1], concat $ [x2] ++ i2 ++ [p2]]
   xknot2 [p1,p2] = [p1,p2]
   
   h = id

{- To make the grammar reusable, its definition has been split up into the
   actual grammar which exposes the start symbol as a parser (oneStructureGrammar)
   and a convenience function which actually runs the grammar on a given input (oneStructure).
-}
oneStructure :: YieldAnalysisAlgorithm Dim1 -> RangeConstructionAlgorithm Dim1
       -> YieldAnalysisAlgorithm Dim2 -> RangeConstructionAlgorithm Dim2 
       -> OneStructure_Algebra Char answer -> String -> [answer]
oneStructure yieldAlg1 rangeAlg1 yieldAlg2 rangeAlg2 algebra inp =
    let z = mk inp
        grammar = oneStructureGrammar yieldAlg1 rangeAlg1 yieldAlg2 rangeAlg2 algebra z
    in axiom z grammar

oneStructureGrammar :: YieldAnalysisAlgorithm Dim1 -> RangeConstructionAlgorithm Dim1
       -> YieldAnalysisAlgorithm Dim2 -> RangeConstructionAlgorithm Dim2 
       -> OneStructure_Algebra Char answer -> Array Int Char -> RichParser Char answer
oneStructureGrammar yieldAlg1 rangeAlg1 yieldAlg2 rangeAlg2 algebra z =
  -- These implicit parameters are used by >>>.
  -- They were introduced to allow for exchanging the algorithms and
  -- they were made implicit so that they don't ruin our nice syntax.
  let ?yieldAlg1 = yieldAlg1
      ?rangeAlg1 = rangeAlg1
      ?yieldAlg2 = yieldAlg2
      ?rangeAlg2 = rangeAlg2
  in let
  
  (nil,left,pair,basepair,base,i1,i2,tstart,knotH,knotK,knotL,knotM,
   aknot1,aknot2,bknot1,bknot2,cknot1,cknot2,dknot1,dknot2,h) = algebra
   
  i = tabulated1 $
      i1 <<< s >>>| id |||
      i2 <<< t >>>| id
  
  rewritePair [p1,p2,s1,s2] = [p1,s1,p2,s2]
  
  s = tabulated1 $
      nil  <<< EPS >>>| id |||
      left <<< b ~~~| s >>>| id |||
      pair <<< p ~~~| s ~~~| s >>>| rewritePair
      
  rewriteTStart [p1,p2,i,t,s] = [i,p1,t,p2,s]
  rewriteKnotH [s,i1,i2,i3,i4,x11,x12,x21,x22] = [i1,x11,i2,x21,i3,x12,i4,x22,s]
  rewriteKnotK [s,i1,i2,i3,i4,i5,i6,x11,x12,x21,x22,x31,x32] = [i1,x11,i2,x21,i3,x12,i4,x31,i5,x22,i6,x32,s]
  rewriteKnotL [s,i1,i2,i3,i4,i5,i6,x11,x12,x21,x22,x31,x32] = [i1,x11,i2,x21,i3,x31,i4,x12,i5,x22,i6,x32,s]
  rewriteKnotM [s,i1,i2,i3,i4,i5,i6,i7,i8,x11,x12,x21,x22,x31,x32,x41,x42] =
          [i1,x11,i2,x21,i3,x31,i4,x12,i5,x41,i6,x22,i7,x32,i8,x42,s]
  t = tabulated1 $
      tstart <<< p ~~~| i ~~~| t ~~~ s >>>| rewriteTStart |||
      knotH <<< s ~~~| i ~~~| i ~~~| i ~~~| i ~~~ xa ~~~ xb >>>| rewriteKnotH |||
      knotK <<< s ~~~| i ~~~| i ~~~| i ~~~| i ~~~| i ~~~| i ~~~ xa ~~~ xb ~~~ xc >>>| rewriteKnotK |||
      knotL <<< s ~~~| i ~~~| i ~~~| i ~~~| i ~~~| i ~~~| i ~~~ xa ~~~ xb ~~~ xc >>>| rewriteKnotL |||
      knotM <<< s ~~~| i ~~~| i ~~~| i ~~~| i ~~~| i ~~~| i ~~~| i ~~~| i ~~~ xa ~~~ xb ~~~ xc ~~~ xd >>>| rewriteKnotM
      
  rewriteXKnot1 [p1,p2,i1,i2,x1,x2] = ([p1,i1,x1],[x2,i2,p2])
  xa = tabulated2 $
      aknot1 <<< p ~~~| i ~~~| i ~~~|| xa >>>|| rewriteXKnot1 |||
      aknot2 <<< p >>>|| id2
      
  xb = tabulated2 $
      bknot1 <<< p ~~~| i ~~~| i ~~~|| xb >>>|| rewriteXKnot1 |||
      bknot2 <<< p >>>|| id2
      
  xc = tabulated2 $
      cknot1 <<< p ~~~| i ~~~| i ~~~|| xb >>>|| rewriteXKnot1 |||
      cknot2 <<< p >>>|| id2
      
  xd = tabulated2 $
      dknot1 <<< p ~~~| i ~~~| i ~~~|| xb >>>|| rewriteXKnot1 |||
      dknot2 <<< p >>>|| id2
  
  b = tabulated1 $
      base <<< 'a' >>>| id |||
      base <<< 'u' >>>| id |||
      base <<< 'c' >>>| id |||
      base <<< 'g' >>>| id
  
  p = tabulated2 $
      basepair <<< ('a', 'u') >>>|| id2 |||
      basepair <<< ('u', 'a') >>>|| id2 |||
      basepair <<< ('c', 'g') >>>|| id2 |||
      basepair <<< ('g', 'c') >>>|| id2 |||
      basepair <<< ('g', 'u') >>>|| id2 |||
      basepair <<< ('u', 'g') >>>|| id2
       
  tabulated1 = table1 z
  tabulated2 = table2 z
  
  in i