{- |
This example is a copy of RGExample with the difference that
(A^*)^i is used in the signature instead of just A^i.
Also, the empty string is used instead of EPS.

The purpose is to have a better relation to the examples in the thesis.
-}
module ADP.Tests.RGExampleStar where

import ADP.Multi.All
import ADP.Multi.Rewriting.All 
                 
              
type RG_Algebra alphabet answer = (
  [alphabet] -> answer,                       -- nil
  answer   -> answer -> answer,               -- left
  answer   -> answer -> answer -> answer,     -- pair
  answer   -> answer -> answer -> answer -> answer -> answer -> answer, -- knot
  answer   -> answer -> answer,               -- knot1
  answer   -> answer,                         -- knot2
  ([alphabet], [alphabet]) -> answer,         -- basepair
  [alphabet] -> answer,                       -- base
  [answer] -> [answer]                        -- h
  )
  
infixl ***
(***) :: (Eq b, Eq c) => RG_Algebra a b -> RG_Algebra a c -> RG_Algebra a (b,c)
alg1 *** alg2 = (nil,left,pair,knot,knot1,knot2,basepair,base,h) where
   (nil',left',pair',knot',knot1',knot2',basepair',base',h') = alg1
   (nil'',left'',pair'',knot'',knot1'',knot2'',basepair'',base'',h'') = alg2
   
   nil a = (nil' a, nil'' a)
   left (b1,b2) (s1,s2) = (left' b1 s1, left'' b2 s2)
   pair (p1,p2) (s11,s21) (s12,s22) = (pair' p1 s11 s12, pair'' p2 s21 s22)
   knot (k11,k21) (k12,k22) (s11,s21) (s12,s22) (s13,s23) (s14,s24) =
        (knot' k11 k12 s11 s12 s13 s14, knot'' k21 k22 s21 s22 s23 s24)
   knot1 (p1,p2) (k1,k2) = (knot1' p1 k1, knot1'' p2 k2)
   knot2 (p1,p2) = (knot2' p1, knot2'' p2)
   basepair a = (basepair' a,  basepair'' a)
   base a = (base' a, base'' a)
   h xs = [ (x1,x2) |
            x1 <- h'  [ y1 | (y1,_)  <- xs]
          , x2 <- h'' [ y2 | (y1,y2) <- xs, y1 == x1]
          ]

data Start = Nil
           | Left' Start Start
           | Pair Start Start Start
           | Knot Start Start Start Start Start Start
           | Knot1 Start Start
           | Knot2 Start
           | BasePair (String, String)
           | Base String
           deriving (Eq, Show)

enum :: RG_Algebra Char Start
enum = (\_->Nil,Left',Pair,Knot,Knot1,Knot2,BasePair,Base,id)

maxBasepairs :: RG_Algebra Char Int
maxBasepairs = (nil,left,pair,knot,knot1,knot2,basepair,base,h) where
   nil _            = 0
   left a b         = a + b
   pair a b c       = a + b + c
   knot a b c d e f = a + b + c + d + e + f
   knot1 a b        = a + b
   knot2 a          = a
   basepair _       = 1
   base _           = 0
   h []             = []
   h xs             = [maximum xs]

maxKnots :: RG_Algebra Char Int
maxKnots = (nil,left,pair,knot,knot1,knot2,basepair,base,h) where
   nil _            = 0
   left _ b         = b
   pair _ b c       = b + c
   knot _ _ c d e f = 1 + c + d + e + f
   knot1 _ _        = 0
   knot2 _          = 0
   basepair _       = 0
   base _           = 0
   h []             = []
   h xs             = [maximum xs]

-- The left part is the structure and the right part the reconstructed input.
prettyprint :: RG_Algebra Char ([String],[String])
prettyprint = (nil,left,pair,knot,knot1,knot2,basepair,base,h) where
   nil _ = ([""],[""])
   left (bl,br) (sl,sr) = 
        (
             [concat $ bl ++ sl],
             [concat $ br ++ sr]
        )
   pair ([p1l,p2l],[p1r,p2r]) (s1l,s1r) (s2l,s2r) = 
        (
             [concat $ [p1l] ++ s1l ++ [p2l] ++ s2l],
             [concat $ [p1r] ++ s1r ++ [p2r] ++ s2r]
        )
   knot ([k11l,k12l],[k11r,k12r]) ([k21l,k22l],[k21r,k22r]) (s1l,s1r) (s2l,s2r) (s3l,s3r) (s4l,s4r) =
        let (k11l',k12l') = square k11l k12l
        in
        (
             [concat $ [k11l'] ++ s1l ++ [k21l] ++ s2l ++ [k12l'] ++ s3l ++ [k22l] ++ s4l],
             [concat $ [k11r] ++ s1r ++ [k21r] ++ s2r ++ [k12r] ++ s3r ++ [k22r] ++ s4r]
        )
   knot1 ([p1l,p2l],[p1r,p2r]) ([k1l,k2l],[k1r,k2r]) =
        (  
             [concat $ [k1l] ++ [p1l], concat $ [p2l] ++ [k2l]],
             [concat $ [k1r] ++ [p1r], concat $ [p2r] ++ [k2r]]
        )
   knot2 (pl,pr) = (pl, pr)
   basepair (b1,b2) = (["(",")"], [b1,b2])
   base b = (["."], [b])
   h = id
   
   square l r = (map (const '[') l, map (const ']') r)

   
rgknot :: RG_Algebra Char answer -> String -> [answer]
rgknot algebra inp =
  let  
  (nil,left,pair,knot,knot1,knot2,basepair,base,h) = algebra
   
  rewritePair, rewriteKnot :: Dim1 
   
  rewritePair [p1,p2,s1,s2] = [p1,s1,p2,s2]
  rewriteKnot [k11,k12,k21,k22,s1,s2,s3,s4] = [k11,s1,k21,s2,k12,s3,k22,s4]
  
  s = tabulated1 $
      yieldSize1 (0,Nothing) $
      nil  <<< "" >>> id1 |||
      left <<< b ~~~ s >>> id1 |||
      pair <<< p ~~~ s ~~~ s >>> rewritePair |||
      knot <<< k ~~~ k ~~~ s ~~~ s ~~~ s ~~~ s >>> rewriteKnot
      ... h
  
  b = tabulated1 $
      base <<< "a" >>> id1 |||
      base <<< "u" >>> id1 |||
      base <<< "c" >>> id1 |||
      base <<< "g" >>> id1
  
  p = tabulated2 $
      basepair <<< ("a", "u") >>> id2 |||
      basepair <<< ("u", "a") >>> id2 |||
      basepair <<< ("c", "g") >>> id2 |||
      basepair <<< ("g", "c") >>> id2 |||
      basepair <<< ("g", "u") >>> id2 |||
      basepair <<< ("u", "g") >>> id2
  
  rewriteKnot1 :: Dim2
  rewriteKnot1 [p1,p2,k1,k2] = ([k1,p1],[p2,k2])
  
  k = tabulated2 $
      yieldSize2 (1,Nothing) (1,Nothing) $
      knot1 <<< p ~~~ k >>> rewriteKnot1 |||
      knot2 <<< p >>> id2
      
  z = mk inp
  tabulated1 = table1 z
  tabulated2 = table2 z
  
  in axiom z s