{- Models the RNA-RNA interaction grammar (RIG) from

"A grammatical approach to RNA–RNA interaction prediction" by Kato et al., 2009

Specifically, example 3 from page 5.

-}
module ADP.Tests.RIGExample where

import ADP.Multi.ElementaryParsers
import ADP.Multi.Combinators
import ADP.Multi.Tabulation
import ADP.Multi.Helpers
import ADP.Multi.Rewriting
                                 
type RIG_Algebra alphabet answer = (
  (EPS,EPS) -> answer,                      -- nil
  alphabet -> answer,             -- base
  (alphabet,alphabet) -> answer,             -- basepair
  answer -> answer -> answer, -- sb1L
  answer -> answer -> answer, -- sb1R
  answer -> answer -> answer, -- sb2L
  answer -> answer -> answer, -- sb2R
  answer -> answer -> answer, -- ib1
  answer -> answer -> answer, -- ib2
  answer -> answer -> answer, -- eb
  answer -> answer -> answer -- w   
  )

  
   
rig :: RIG_Algebra Char answer -> (String,String) -> [answer]
rig algebra (inp1,inp2) =
  let  
  (nil,base,basepair,sb1L,sb1R,sb2L,sb2R,ib1,ib2,eb,w) = algebra
      
  rewriteSb1L [b,a1,a2] = ([b,a1],[a2])
  rewriteSb1R [b,a1,a2] = ([a1,b],[a2])
  rewriteSb2L [b,a1,a2] = ([a1],[b,a2])
  rewriteSb2R [b,a1,a2] = ([a1],[a2,b])      
  rewriteIb1 [p1,p2,a1,a2] = ([p1,a1,p2],[a2])
  rewriteIb2 [p1,p2,a1,a2] = ([a1],[p1,a2,p2])
  rewriteEb [p1,p2,a1,a2] = ([p1,a1],[a2,p2])
  rewriteW [a11,a12,a21,a22] = ([a11,a21],[a22,a12])
  a = tabulated2 $
      nil <<< (EPS,EPS) >>>|| id2 |||
      sb1L <<< b ~~~| a >>>|| rewriteSb1L |||
      sb1R <<< b ~~~| a >>>|| rewriteSb1R |||
      sb2L <<< b ~~~| a >>>|| rewriteSb2L |||
      sb2R <<< b ~~~| a >>>|| rewriteSb2R |||
      ib1 <<< p ~~~| a >>>|| rewriteIb1 |||
      ib2 <<< p ~~~| a >>>|| rewriteIb2 |||
      eb  <<< p ~~~| a >>>|| rewriteEb |||
      w   <<< a ~~~| a >>>|| rewriteW 
      -- FIXME Won't work due to recursion and min yield of aa = 0
      -- Is this grammar actually semantically unambigous??
      
  p = tabulated2 $
      basepair <<< ('a', 'u') >>>|| id2 |||
      basepair <<< ('u', 'a') >>>|| id2 |||
      basepair <<< ('c', 'g') >>>|| id2 |||
      basepair <<< ('g', 'c') >>>|| id2 |||
      basepair <<< ('g', 'u') >>>|| id2 |||
      basepair <<< ('u', 'g') >>>|| id2
      
  b = tabulated1 $
      base <<< 'a' >>>| id |||
      base <<< 'u' >>>| id |||
      base <<< 'c' >>>| id |||
      base <<< 'g' >>>| id
  
  z = mkTwoTrack inp1 inp2
  tabulated1 = table1 z
  tabulated2 = table2 z
  
  in axiomTwoTrack z inp1 inp2 a