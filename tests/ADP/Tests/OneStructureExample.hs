{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ImplicitParams #-}

{- "Topology and prediction of RNA pseudoknots" by Reidys et al., 2011

-}
module ADP.Tests.OneStructureExample where

import Data.Typeable
import Data.Data
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
  answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer, -- knot1
  answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer, -- knot2
  answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer, -- knot3
  answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer -> answer, -- knot4
  answer -> answer -> answer -> answer -> answer, -- xknot1
  answer -> answer,                           -- xknot2
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
       | Knot1 T T T T T T T
       | Knot2 T T T T T T T T T T
       | Knot3 T T T T T T T T T T
       | Knot4 T T T T T T T T T T T T T
       | XKnot1 T T T T
       | XKnot2 T
       deriving (Eq, Show, Data, Typeable)

enum :: OneStructure_Algebra Char T
enum = (nil,left,pair,basepair,base,i1,i2,tstart,knot1,knot2,knot3,knot4,xknot1,xknot2,h) where
   nil _     = Nil
   left      = Left'
   pair      = Pair 
   basepair  = BasePair
   base      = Base
   i1        = I1
   i2        = I2
   tstart    = TStart
   knot1     = Knot1
   knot2     = Knot2
   knot3     = Knot3
   knot4     = Knot4
   xknot1    = XKnot1
   xknot2    = XKnot2
   h         = id 
   
oneStructure :: YieldAnalysisAlgorithm Dim1 -> RangeConstructionAlgorithm Dim1
       -> YieldAnalysisAlgorithm Dim2 -> RangeConstructionAlgorithm Dim2 
       -> OneStructure_Algebra Char answer -> String -> [answer]
oneStructure yieldAlg1 rangeAlg1 yieldAlg2 rangeAlg2 algebra inp =
  -- These implicit parameters are used by >>>.
  -- They were introduced to allow for exchanging the algorithms and
  -- they were made implicit so that they don't ruin our nice syntax.
  let ?yieldAlg1 = yieldAlg1
      ?rangeAlg1 = rangeAlg1
      ?yieldAlg2 = yieldAlg2
      ?rangeAlg2 = rangeAlg2
  in let
  
  (nil,left,pair,basepair,base,i1,i2,tstart,knot1,knot2,knot3,knot4,xknot1,xknot2,h) = algebra
   
  i = tabulated1 $
      i1 <<< s >>>| id |||
      i2 <<< t >>>| id
  
  rewritePair [p1,p2,s1,s2] = [p1,s1,p2,s2]
  
  s = tabulated1 $
      nil  <<< EPS >>>| id |||
      left <<< b ~~~| s >>>| id |||
      pair <<< p ~~~| s ~~~| s >>>| rewritePair
      
  rewriteTStart [p1,p2,i,t,s] = [i,p1,t,p2,s]
  rewriteKnot1 [s,i1,i2,i3,i4,x11,x12,x21,x22] = [i1,x11,i2,x21,i3,x12,i4,x22,s]
  rewriteKnot2 [s,i1,i2,i3,i4,i5,i6,x11,x12,x21,x22,x31,x32] = [i1,x11,i2,x21,i3,x12,i4,x31,i5,x22,i6,x32,s]
  rewriteKnot3 [s,i1,i2,i3,i4,i5,i6,x11,x12,x21,x22,x31,x32] = [i1,x11,i2,x21,i3,x31,i4,x12,i5,x22,i6,x32,s]
  rewriteKnot4 [s,i1,i2,i3,i4,i5,i6,i7,i8,x11,x12,x21,x22,x31,x32,x41,x42] =
          [i1,x11,i2,x21,i3,x31,i4,x12,i5,x41,i6,x22,i7,x32,i8,x42,s]
  t = tabulated1 $
      tstart <<< p ~~~| i ~~~| t ~~~ s >>>| rewriteTStart |||
      knot1 <<< s ~~~| i ~~~| i ~~~| i ~~~| i ~~~ x ~~~ x >>>| rewriteKnot1 |||
      knot2 <<< s ~~~| i ~~~| i ~~~| i ~~~| i ~~~| i ~~~| i ~~~ x ~~~ x ~~~ x >>>| rewriteKnot2 |||
      knot3 <<< s ~~~| i ~~~| i ~~~| i ~~~| i ~~~| i ~~~| i ~~~ x ~~~ x ~~~ x >>>| rewriteKnot3 |||
      knot4 <<< s ~~~| i ~~~| i ~~~| i ~~~| i ~~~| i ~~~| i ~~~| i ~~~| i ~~~ x ~~~ x ~~~ x ~~~ x >>>| rewriteKnot4
      
  rewriteXKnot1 [p1,p2,i1,i2,x1,x2] = ([p1,i1,x1],[x2,i2,p2])
  rewriteXKnot1 a = error (show a)
  x = tabulated2 $
      xknot1 <<< p ~~~| i ~~~| i ~~~|| x >>>|| rewriteXKnot1 |||
      xknot2 <<< p >>>|| id2
  
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
       
  z = mk inp
  tabulated1 = table1 z
  tabulated2 = table2 z
  
  in axiom z i