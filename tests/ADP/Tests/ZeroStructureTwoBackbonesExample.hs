{- |
   This example implements the grammar for 0-structures over two backbones from
   "Topology of RNA-RNA interaction structures" by Andersen et al., 2012
   
   It uses the 1-structure grammar from
   "Topology and prediction of RNA pseudoknots" by Reidys et al., 2011
   by importing it from ADP.Tests.OneStructureExample
-}
module ADP.Tests.ZeroStructureTwoBackbonesExample where

import Data.Array

import ADP.Multi.All
import ADP.Multi.Rewriting.All
import qualified ADP.Tests.OneStructureExample as One

{- There are two ans types so that the enum
   algebra can be written (because ADTs aren't extensible).
   For algebras with numeric ans types it wouldn't matter
   and we'd only need one type.
-} 
type ZeroStructureTwoBackbones_Algebra alphabet ansOne ans = (
  One.OneStructure_Algebra alphabet ansOne,
  ans    -> ansOne -> ansOne -> ans,     -- i1
  ansOne -> ansOne -> ans,               -- i2
  ans    -> ans    -> ans,               -- pt1
  ans    -> ans    -> ans,               -- pt2
  ansOne -> ansOne -> ans -> ans -> ans, -- t1
  ansOne -> ansOne -> ans -> ans -> ans, -- t2
  ansOne -> ansOne -> ans -> ans -> ans, -- t3
  ansOne -> ansOne -> ansOne -> ansOne -> ans -> ans -> ans -> ans,   -- t4
  ansOne -> ansOne -> ansOne -> ansOne -> ansOne -> ansOne -> ans -> ans -> ans -> ans -> ans, -- t5
  ansOne -> ansOne -> ansOne -> ansOne -> ans -> ans -> ans -> ans,   -- t6
  ansOne -> ansOne -> ansOne -> ansOne -> ans -> ans -> ans -> ans,   -- t7
  ansOne -> ansOne -> ans -> ans -> ans, -- hs2
  ans -> ans -> ans -> ans -> ans,       -- h1
  ans -> ans,                            -- h2
  ans -> ansOne -> ansOne -> ans -> ans, -- g1
  ans -> ans,                            -- g2
  ans -> ans -> ans,                     -- ub1
  EPS -> ans,                            -- ub2
  alphabet -> ans,                       -- base
  (alphabet, alphabet) -> ans,           -- basepair
  [ans] -> [ans]                         -- h
  )

data T = OneStructure One.T
       | I1 T One.T One.T
       | I2 One.T One.T
       | PT1 T T
       | PT2 T T
       | T1 One.T One.T T T
       | T2 One.T One.T T T
       | T3 One.T One.T T T
       | T4 One.T One.T One.T One.T T T T
       | T5 One.T One.T One.T One.T One.T One.T T T T T
       | T6 One.T One.T One.T One.T T T T
       | T7 One.T One.T One.T One.T T T T
       | Hs2 One.T One.T T T
       | H1 T T T T
       | H2 T
       | G1 T One.T One.T T
       | G2 T 
       | Ub1 T T
       | Ub2
       | Base Char
       | BasePair (Char, Char)
       deriving (Eq, Show)

enum :: ZeroStructureTwoBackbones_Algebra Char One.T T
enum = (One.enum,I1,I2,PT1,PT2,T1,T2,T3,T4,T5,T6,T7
       ,Hs2,H1,H2,G1,G2,Ub1,\_->Ub2,Base,BasePair,id)

{- To make the grammar reusable, its definition has been split
   up into the actual grammar which exposes the start symbol
   as a parser (zeroStructureTwoBackbonesGrammar) and a
   convenience function which actually runs the grammar on
   a given input (zeroStructureTwoBackbones).
-}
zeroStructureTwoBackbones :: ZeroStructureTwoBackbones_Algebra Char ansOne ans 
                          -> (String,String) -> [ans]
zeroStructureTwoBackbones algebra (inp1,inp2) =
    let z = mkTwoTrack inp1 inp2
        grammar = zeroStructureTwoBackbonesGrammar algebra z
    in axiomTwoTrack z inp1 inp2 grammar

zeroStructureTwoBackbonesGrammar :: ZeroStructureTwoBackbones_Algebra Char ansOne ans 
                                 -> Array Int Char -> RichParser Char ans
zeroStructureTwoBackbonesGrammar algebra z =
  let  
  (oneStructureAlgebra,i1,i2,pt1,pt2,t1,t2,t3,t4,t5,
   t6,t7,hs2,h1,h2,g1,g2,ub1,ub2,base,basepair,h') = algebra
  
  one = One.oneStructureGrammar oneStructureAlgebra z
  
  rewriteI1, rewriteI2, rewritePT1, rewritePT2 :: Dim2
  
  rewriteI1 [pt1,pt2,one1,one2] = ([pt1,one1],[one2,pt2])
  rewriteI2 [one1,one2] = ([one1],[one2])
  i = tabulated2 $
      i1 <<< pt ~~~ one ~~~ one >>> rewriteI1 |||
      i2 <<< one ~~~ one >>> rewriteI2
  
  rewritePT1 [t1,t2,i1,i2] = ([i1,t1],[t2,i2])
  rewritePT2 [h1,h2,i1,i2] = ([i1,h1],[h2,i2])
  pt = tabulated2 $
       yieldSize2 (1,Nothing) (1,Nothing) $
       pt1 <<< t ~~~ i >>> rewritePT1 |||
       pt2 <<< h ~~~ i >>> rewritePT2
       
  rewriteT1, rewriteT2, rewriteT3, rewriteT4, rewriteT5, rewriteT6, rewriteT7 :: Dim2
       
  rewriteT1 [one1,one2,hs11,hs12,hs21,hs22] = ([hs11,one1,hs21],[hs12,one2,hs22])
  rewriteT2 [one1,one2,g1,g2,hs1,hs2] = ([g1,one1,hs1,one2,g2],[hs2])
  rewriteT3 [one1,one2,hs1,hs2,g1,g2] = ([hs1],[g1,one1,hs2,one2,g2])
  rewriteT4 [one1,one2,one3,one4,g11,g12,hs1,hs2,g21,g22]
        = ([g11,one1,hs1,one2,g12],[g21,one3,hs2,one4,g22])
  rewriteT5 [one1,one2,one3,one4,one5,one6,g11,g12,hs11,hs12,hs21,hs22,g21,g22]
        = ([g11,one1,hs11,one2,hs21,one3,g12],[g21,one4,hs12,one5,hs22,one6,g22])
  rewriteT6 [one1,one2,one3,one4,g1,g2,hs11,hs12,hs21,hs22] 
        = ([g1,one1,hs11,one2,hs21,one3,g2],[hs12,one4,hs22])
  rewriteT7 [one1,one2,one3,one4,hs11,hs12,hs21,hs22,g1,g2] 
        = ([hs11,one1,hs21],[g1,one2,hs12,one3,hs22,one4,g2])  
  t = tabulated2 $
      t1 <<< one ~~~ one ~~~ hs  ~~~ hs >>> rewriteT1 |||
      t2 <<< one ~~~ one ~~~ g   ~~~ hs >>> rewriteT2 |||
      t3 <<< one ~~~ one ~~~ hs  ~~~ g  >>> rewriteT3 |||
      t4 <<< one ~~~ one ~~~ one ~~~ one ~~~ g ~~~ hs ~~~ g >>> rewriteT4 |||
      t5 <<< one ~~~ one ~~~ one ~~~ one ~~~ one ~~~ one ~~~ g ~~~ hs ~~~ hs ~~~ g >>> rewriteT5 |||
      t6 <<< one ~~~ one ~~~ one ~~~ one ~~~ g ~~~ hs ~~~ hs >>> rewriteT6 |||
      t7 <<< one ~~~ one ~~~ one ~~~ one ~~~ hs ~~~ hs ~~~ g >>> rewriteT7
  
  rewriteHs2, rewriteH1, rewriteG1 :: Dim2
  
  rewriteHs2 [one1,one2,h1,h2,hs1,hs2] = ([h1,one1,hs1],[hs2,one2,h2])
  hs = tabulated2 $
       yieldSize2 (1,Nothing) (1,Nothing) $
       h |||
       hs2 <<< one ~~~ one ~~~ h ~~~ hs >>> rewriteHs2
       
  rewriteH1 [p1,p2,ub1,ub2,h1,h2] = ([p1,ub1,h1],[h2,ub2,p2])
  h = tabulated2 $
      yieldSize2 (1,Nothing) (1,Nothing) $
      h1 <<< p ~~~ ub ~~~ ub ~~~ h >>> rewriteH1 |||
      h2 <<< p >>> id2
  
  rewriteG1 [p1,p2,one1,one2,g1,g2] = ([p1,one1,g1],[g2,one2,p2])
  g = tabulated2 $
      yieldSize2 (1,Nothing) (1,Nothing) $
      g1 <<< p ~~~ one ~~~ one ~~~ g >>> rewriteG1 |||
      g2 <<< p >>> id2
  
  ub = tabulated1 $
      yieldSize1 (0,Nothing) $
      ub1 <<< b ~~~ ub >>> id1 |||
      ub2 <<< EPS >>> id1
  
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