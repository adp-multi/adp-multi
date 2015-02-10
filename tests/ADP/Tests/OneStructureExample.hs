-- | This example implements the 1-structure grammar from
--   "Topology and prediction of RNA pseudoknots" by Reidys et al., 2011
module ADP.Tests.OneStructureExample where

import Data.Array

import ADP.Multi.All
import ADP.Multi.Rewriting.All
                          
type OneStructure_Algebra alphabet ans = (
  EPS -> ans,                        -- nil
  ans -> ans -> ans,                 -- left
  ans -> ans -> ans -> ans,          -- pair
  (alphabet, alphabet) -> ans,       -- basepair
  alphabet -> ans,                   -- base
  ans -> ans,                        -- i1
  ans -> ans,                        -- i2
  ans -> ans -> ans -> ans -> ans,   -- tstart
  ans -> ans -> ans -> ans -> ans -> ans -> ans -> ans, -- knotH
  ans -> ans -> ans -> ans -> ans -> ans -> ans -> ans -> ans -> ans -> ans, -- knotK
  ans -> ans -> ans -> ans -> ans -> ans -> ans -> ans -> ans -> ans -> ans, -- knotL
  ans -> ans -> ans -> ans -> ans -> ans -> ans -> ans -> ans -> ans -> ans -> ans -> ans -> ans, -- knotM
  ans -> ans -> ans -> ans -> ans,   -- aknot1
  ans -> ans,                        -- aknot2
  ans -> ans -> ans -> ans -> ans,   -- bknot1
  ans -> ans,                        -- bknot2
  ans -> ans -> ans -> ans -> ans,   -- cknot1
  ans -> ans,                        -- cknot2
  ans -> ans -> ans -> ans -> ans,   -- dknot1
  ans -> ans,                        -- dknot2
  [ans] -> [ans]                     -- h
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
   
-- | dot-bracket
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
   
   xknot1 parenL parenR i1 i2 [x1,x2] = 
        [concat $ [parenL] ++ i1 ++ [x1], concat $ [x2] ++ i2 ++ [parenR]]
      
   h = id
   
-- | reconstructed input
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
   It is reused in ZeroStructureTwoBackbonesExample.hs
-}
oneStructure :: OneStructure_Algebra Char ans -> String -> [ans]
oneStructure algebra inp =
    let z = mk inp
        grammar = oneStructureGrammar algebra z
    in axiom z grammar

oneStructureGrammar :: OneStructure_Algebra Char ans -> Array Int Char -> RichParser Char ans
oneStructureGrammar algebra z =
  let  
  (nil,left,pair,basepair,base,i1,i2,tstart,knotH,knotK,knotL,knotM,
   aknot1,aknot2,bknot1,bknot2,cknot1,cknot2,dknot1,dknot2,h) = algebra
   
  i = tabulated1 $
      i1 <<< s >>> id1 |||
      i2 <<< t >>> id1
  
  rewritePair, rewriteTStart, rewriteKnotH, rewriteKnotK, rewriteKnotL, rewriteKnotM :: Dim1
  
  rewritePair [p1,p2,s1,s2] = [p1,s1,p2,s2]
  
  s = tabulated1 $
      yieldSize1 (0, Nothing) $
      nil  <<< EPS 0 >>> id1 |||
      left <<< b ~~~ s >>> id1 |||
      pair <<< p ~~~ s ~~~ s >>> rewritePair
      
  rewriteTStart [p1,p2,i,t,s] = [i,p1,t,p2,s]
  rewriteKnotH [s,i1,i2,i3,i4,x11,x12,x21,x22] =
        [i1,x11,i2,x21,i3,x12,i4,x22,s]
  rewriteKnotK [s,i1,i2,i3,i4,i5,i6,x11,x12,x21,x22,x31,x32] = 
        [i1,x11,i2,x21,i3,x12,i4,x31,i5,x22,i6,x32,s]
  rewriteKnotL [s,i1,i2,i3,i4,i5,i6,x11,x12,x21,x22,x31,x32] = 
        [i1,x11,i2,x21,i3,x31,i4,x12,i5,x22,i6,x32,s]
  rewriteKnotM [s,i1,i2,i3,i4,i5,i6,i7,i8,x11,x12,x21,x22,x31,x32,x41,x42] =
        [i1,x11,i2,x21,i3,x31,i4,x12,i5,x41,i6,x22,i7,x32,i8,x42,s]
  t = tabulated1 $
      yieldSize1 (2, Nothing) $
      tstart <<< p ~~~ i ~~~ t ~~~ s >>> rewriteTStart |||
      knotH  <<< s ~~~ i ~~~ i ~~~ i ~~~ i ~~~ xa ~~~ xb >>> rewriteKnotH |||
      knotK  <<< s ~~~ i ~~~ i ~~~ i ~~~ i ~~~ i ~~~ i ~~~ xa ~~~ xb ~~~ xc >>> rewriteKnotK |||
      knotL  <<< s ~~~ i ~~~ i ~~~ i ~~~ i ~~~ i ~~~ i ~~~ xa ~~~ xb ~~~ xc >>> rewriteKnotL |||
      knotM  <<< s ~~~ i ~~~ i ~~~ i ~~~ i ~~~ i ~~~ i ~~~ i ~~~ i ~~~ xa ~~~ xb ~~~ xc ~~~ xd >>> rewriteKnotM
      
  rewriteXKnot1 :: Dim2      
  rewriteXKnot1 [p1,p2,i1,i2,x1,x2] = ([p1,i1,x1],[x2,i2,p2])
  
  xa = tabulated2 $
      yieldSize2 (1, Nothing) (1, Nothing) $
      aknot1 <<< p ~~~ i ~~~ i ~~~ xa >>> rewriteXKnot1 |||
      aknot2 <<< p >>> id2
      
  xb = tabulated2 $
      yieldSize2 (1, Nothing) (1, Nothing) $
      bknot1 <<< p ~~~ i ~~~ i ~~~ xb >>> rewriteXKnot1 |||
      bknot2 <<< p >>> id2
      
  xc = tabulated2 $
      cknot1 <<< p ~~~ i ~~~ i ~~~ xb >>> rewriteXKnot1 |||
      cknot2 <<< p >>> id2
      
  xd = tabulated2 $
      dknot1 <<< p ~~~ i ~~~ i ~~~ xb >>> rewriteXKnot1 |||
      dknot2 <<< p >>> id2
  
  b = tabulated1 $
      base <<< 'a' >>> id1 |||
      base <<< 'u' >>> id1 |||
      base <<< 'c' >>> id1 |||
      base <<< 'g' >>> id1
  
  p = tabulated2 $
      basepair <<< ('a', 'u') >>> id2 |||
      basepair <<< ('u', 'a') >>> id2 |||
      basepair <<< ('c', 'g') >>> id2 |||
      basepair <<< ('g', 'c') >>> id2 |||
      basepair <<< ('g', 'u') >>> id2 |||
      basepair <<< ('u', 'g') >>> id2
       
  tabulated1 = table1 z
  tabulated2 = table2 z
  
  in i
