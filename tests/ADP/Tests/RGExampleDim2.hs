{-# LANGUAGE DeriveDataTypeable #-}

{- |
The same as RGExample.hs but all 1-dim nonterminals are encoded
as 2-dim nonterminals.
-}
module ADP.Tests.RGExampleDim2 where

import Data.Typeable
import Data.Data
import Data.Array
import ADP.Multi.All
import ADP.Multi.Rewriting.All
                                 
type RG_Algebra alphabet answer = (
  (EPS,EPS) -> answer,                        -- nil
  EPS -> answer   -> answer -> answer,               -- left
  EPS -> answer   -> answer -> answer -> answer,     -- pair
  EPS -> answer   -> answer -> answer -> answer -> answer -> answer -> answer, -- knot
  answer   -> answer -> answer,               -- knot1
  answer   -> answer,                         -- knot2
  (alphabet, alphabet) -> answer,             -- basepair
  (EPS, alphabet) -> answer,                  -- base
  [answer] -> [answer]                        -- h
  )
  
infixl ***
(***) :: (Eq b, Eq c) => RG_Algebra a b -> RG_Algebra a c -> RG_Algebra a (b,c)
alg1 *** alg2 = (nil,left,pair,knot,knot1,knot2,basepair,base,h) where
   (nil',left',pair',knot',knot1',knot2',basepair',base',h') = alg1
   (nil'',left'',pair'',knot'',knot1'',knot2'',basepair'',base'',h'') = alg2

   nil a = (nil' a, nil'' a)
   left e (b1,b2) (s1,s2) = (left' e b1 s1, left'' e b2 s2)
   pair e (p1,p2) (s11,s21) (s12,s22) = (pair' e p1 s11 s12, pair'' e p2 s21 s22)
   knot e (k11,k21) (k12,k22) (s11,s21) (s12,s22) (s13,s23) (s14,s24) =
        (knot' e k11 k12 s11 s12 s13 s14, knot'' e k21 k22 s21 s22 s23 s24)
   knot1 (p1,p2) (k1,k2) = (knot1' p1 k1, knot1'' p2 k2)
   knot2 (p1,p2) = (knot2' p1, knot2'' p2)
   basepair a = (basepair' a,  basepair'' a)
   base a = (base' a, base'' a)
   h xs = [ (x1,x2) |
            x1 <- h'  [ y1 | (y1,_)  <- xs]
          , x2 <- h'' [ y2 | (y1,y2) <- xs, y1 == x1]
          ]


-- This data type is used only for the enum algebra.
-- The type allows invalid trees which would be impossible to build
-- with the given grammar rules.
-- As an additional (programming) error check, a second debug enum algebra checks
-- the types via pattern-matching.
data Start = Nil
           | Left' EPS Start Start
           | Pair EPS Start Start Start
           | Knot EPS Start Start Start Start Start Start
           | Knot1 Start Start
           | Knot2 Start
           | BasePair (Char, Char)
           | Base (EPS, Char)
           deriving (Eq, Show, Data, Typeable)

-- without consistency checks
enum :: RG_Algebra Char Start
enum = (\_->Nil,Left',Pair,Knot,Knot1,Knot2,BasePair,Base,id)

-- with consistency checks
enumDebug :: RG_Algebra Char Start
enumDebug = (nil,left,pair,knot,knot1,knot2,basepair,base,h) where

   s' = [Nil, Left'{}, Pair{}, Knot{}]
   k' = [Knot1 {}, Knot2 {}]

   nil _ = Nil
   left e b@(Base _) s 
        | s `isOf` s' = Left' e b s
        
   pair e p@(BasePair _) s1 s2 
        | [s1,s2] `areOf` s' = Pair e p s1 s2
        
   knot e k1 k2 s1 s2 s3 s4 
        | [k1,k2] `areOf` k' && [s1,s2,s3,s4] `areOf` s' = Knot e k1 k2 s1 s2 s3 s4
        
   knot1 p@(BasePair _) k 
        | k `isOf` k' = Knot1 p k
        
   knot2 p@(BasePair _) = Knot2 p
   basepair             = BasePair
   base                 = Base
   h                    = id
   
   isOf l r = toConstr l `elem` map toConstr r
   areOf l r = all (`isOf` r) l
   
maxBasepairs :: RG_Algebra Char Int
maxBasepairs = (nil,left,pair,knot,knot1,knot2,basepair,base,h) where
   nil _            = 0
   left _ a b         = a + b
   pair _ a b c       = a + b + c
   knot _ a b c d e f = a + b + c + d + e + f
   knot1 a b        = a + b
   knot2 a          = a
   basepair _       = 1
   base _           = 0
   h []             = []
   h xs             = [maximum xs]

maxKnots :: RG_Algebra Char Int
maxKnots = (nil,left,pair,knot,knot1,knot2,basepair,base,h) where
   nil _            = 0
   left _ _ b         = b
   pair _ _ b c       = b + c
   knot _ _ _ c d e f = 1 + c + d + e + f
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
   left _ (bl,br) (sl,sr) = 
        (
             [concat $ bl ++ sl],
             [concat $ br ++ sr]
        )
   pair _ ([p1l,p2l],[p1r,p2r]) (s1l,s1r) (s2l,s2r) = 
        (
             [concat $ [p1l] ++ s1l ++ [p2l] ++ s2l],
             [concat $ [p1r] ++ s1r ++ [p2r] ++ s2r]
        )
   knot _ ([k11l,k12l],[k11r,k12r]) ([k21l,k22l],[k21r,k22r]) (s1l,s1r) (s2l,s2r) (s3l,s3r) (s4l,s4r) =
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
   basepair (b1,b2) = (["(",")"], [[b1],[b2]])
   base (_,b) = (["."], [[b]])
   h = id
   
   square l r = (map (const '[') l, map (const ']') r)
   
rgknot :: RG_Algebra Char answer -> String -> [answer]
rgknot algebra inp =
  let  
  (nil,left,pair,knot,knot1,knot2,basepair,base,h) = algebra
  
  s2,s3,s4,p',k1,k2 :: Dim2
    
  -- all s are 1-dim simulated as 2-dim
  s2 [e,b1,b2,s1,s2] = ([e],[b1,b2,s1,s2])
  s3 [e,p1,p2,s11,s12,s21,s22] = ([e],[p1,s11,s12,p2,s21,s22])
  s4 [e,k11,k12,k21,k22,s11,s12,s21,s22,s31,s32,s41,s42] = 
        ([e],[k11,s11,s12,k21,s21,s22,k12,s31,s32,k22,s41,s42])
  
  s = tabulated2 $
      yieldSize2 (0,Nothing) (0,Nothing) $
      nil <<< (EPS 0,EPS 0) >>> id2 |||
      left <<< EPS 0 ~~~ b ~~~ s >>> s2 |||
      pair <<< EPS 0 ~~~ p ~~~ s ~~~ s >>> s3 |||
      knot <<< EPS 0 ~~~ k ~~~ k ~~~ s ~~~ s ~~~ s ~~~ s >>> s4 
      ... h
      
  b = tabulated2 $
      base <<< (EPS 0, 'a') >>> id2 |||
      base <<< (EPS 0, 'u') >>> id2 |||
      base <<< (EPS 0, 'c') >>> id2 |||
      base <<< (EPS 0, 'g') >>> id2
  
  p' [c1,c2] = ([c1],[c2])
  p = tabulated2 $
      basepair <<< ('a', 'u') >>> p' |||
      basepair <<< ('u', 'a') >>> p' |||
      basepair <<< ('c', 'g') >>> p' |||
      basepair <<< ('g', 'c') >>> p' |||
      basepair <<< ('g', 'u') >>> p' |||
      basepair <<< ('u', 'g') >>> p'
  
  k1 [p1,p2,k1,k2] = ([k1,p1],[p2,k2])
  k2 [p1,p2] = ([p1],[p2])
  
  k = tabulated2 $
      yieldSize2 (1,Nothing) (1,Nothing) $
      knot1 <<< p ~~~ k >>> k1 |||
      knot2 <<< p >>> k2
      
  z = mk inp
  tabulated2 = table2 z
  
  axiom' :: Array Int a -> RichParser a b -> [b]
  axiom' z (_,ax) =
      let (_,l) = bounds z
      in ax z [0,0,0,l]
  in axiom' z s
