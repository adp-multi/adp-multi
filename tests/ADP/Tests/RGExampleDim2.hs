{-# LANGUAGE DeriveDataTypeable #-}

{-
Example using the Reeder&Giegerich class of pseudoknots.

The grammar was taken from:

Markus E. Nebel and Frank Weinberg. Algebraic and Combinatorial Properties of Common
RNA Pseudoknot Classes with Applications. (submitted), 2012.

The original algorithm (not in grammar form) can be found in:

Jens Reeder and Robert Giegerich. Design, implementation and evaluation of a practical
pseudoknot folding algorithm based on thermodynamics. BMC Bioinformatics, 5:104, 2004.
-}
module ADP.Tests.RGExampleDim2 where

{-
S -> € | BS | P_1 S P_2 S | K_1^1 S K_1^2 S K_2^1 S K_2^2 S
[K_1,K_2] -> [K_1 P_1, P_2 K_2] | [P_1, P_2]
[P_1,P_2] -> [a,u] | [u,a] | [g,c] | [c,g] | [g,u] | [u,g]
B -> a | u | c | g
-}

import Data.Array (bounds)
import qualified Control.Arrow as A
import Data.Typeable
import Data.Data
import Data.Array
import ADP.Multi.Parser
import ADP.Multi.SimpleParsers
import ADP.Multi.Combinators
import ADP.Multi.Tabulation
import ADP.Multi.Helpers
import ADP.Multi.Rewriting
                                 
type RG_Algebra alphabet answer = (
  (EPS,EPS) -> answer,                               -- nil
  answer   -> answer -> answer,               -- left
  answer   -> answer -> answer -> answer,     -- pair
  answer   -> answer -> answer -> answer -> answer -> answer -> answer, -- knot
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
   
   nil = nil' A.&&& nil''
   left b s = (left', left'') **** b **** s
   pair p s1 s2 = (pair', pair'') **** p **** s1 **** s2
   knot k1 k2 s1 s2 s3 s4 = (knot', knot'') **** k1 **** k2 **** s1 **** s2 **** s3 **** s4
   knot1 p k = (knot1', knot1'') **** p **** k
   knot2 p = (knot2', knot2'') **** p
   basepair = basepair' A.&&& basepair''
   base = base' A.&&& base''
   h xs = [ (x1,x2) |
            x1 <- h'  [ y1 | (y1,_)  <- xs]
          , x2 <- h'' [ y2 | (y1,y2) <- xs, y1 == x1]
          ]

   (****) = uncurry (A.***)

{-
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
-}

-- This data type is used only for the enum algebra.
-- The type allows invalid trees which would be impossible to build
-- with the given grammar rules.
-- As an additional (programming) error check, a second debug enum algebra checks
-- the types via pattern-matching.
data Start = Nil
           | Left' Start Start
           | Pair Start Start Start
           | Knot Start Start Start Start Start Start
           | Knot1 Start Start
           | Knot2 Start
           | BasePair (Char, Char)
           | Base (EPS, Char)
           deriving (Eq, Show, Data, Typeable)

-- without consistency checks
enum :: RG_Algebra Char Start
enum = (nil,left,pair,knot,knot1,knot2,basepair,base,h) where
   nil _     = Nil
   left      = Left'
   pair      = Pair 
   knot      = Knot 
   knot1     = Knot1 
   knot2     = Knot2
   basepair  = BasePair
   base      = Base
   h         = id 

-- with consistency checks
enumDebug :: RG_Algebra Char Start
enumDebug = (nil,left,pair,knot,knot1,knot2,basepair,base,h) where

   s' = [Nil, Left'{}, Pair{}, Knot{}]
   k' = [Knot1 {}, Knot2 {}]

   nil _ = Nil
   left  b@(Base _) s 
        | s `isOf` s' = Left' b s
        
   pair  p@(BasePair _) s1 s2 
        | [s1,s2] `areOf` s' = Pair p s1 s2
        
   knot k1 k2 s1 s2 s3 s4 
        | [k1,k2] `areOf` k' && [s1,s2,s3,s4] `areOf` s' = Knot k1 k2 s1 s2 s3 s4
        
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

-- TODO don't need [String] here as it's all dim2, use (String,String) instead
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
   basepair (b1,b2) = (["(",")"], [[b1],[b2]])
   base (EPS,b) = (["."], [[b]])
   h = id
   
   square l r = (map (const '[') l, map (const ']') r)
   
rgknot :: RG_Algebra Char answer -> String -> [answer]
rgknot algebra inp =
  let  
  (nil,left,pair,knot,knot1,knot2,basepair,base,h) = algebra
  
  s1,s2,s3,s4,p',k1,k2 :: Dim2
    
  -- all s are 1-dim simulated as 2-dim
  s1 [c1,c2] = ([],[c1,c2])
  s2 [b1,b2,s1,s2] = ([],[b1,b2,s1,s2])
  s3 [p1,p2,s11,s12,s21,s22] = ([],[p1,s11,s12,p2,s21,s22])
  s4 [k11,k12,k21,k22,s11,s12,s21,s22,s31,s32,s41,s42] = 
        ([],[k11,s11,s12,k21,s21,s22,k12,s31,s32,k22,s41,s42])
  
  s = tabulated2 $
      nil <<< (EPS,EPS) >>>|| s1 |||
      left <<< b ~~~|| s >>>|| s2 |||
      pair <<< p ~~~|| s ~~~|| s >>>|| s3 |||
      knot <<< k ~~~ k ~~~|| s ~~~|| s ~~~|| s ~~~|| s >>>|| s4 
      ... h
      
  b = tabulated2 $
      base <<< (EPS, 'a') >>>|| s1 |||
      base <<< (EPS, 'u') >>>|| s1 |||
      base <<< (EPS, 'c') >>>|| s1 |||
      base <<< (EPS, 'g') >>>|| s1
  
  p' [c1,c2] = ([c1],[c2])
  p = tabulated2 $
      basepair <<< ('a', 'u') >>>|| p' |||
      basepair <<< ('u', 'a') >>>|| p' |||
      basepair <<< ('c', 'g') >>>|| p' |||
      basepair <<< ('g', 'c') >>>|| p' |||
      basepair <<< ('g', 'u') >>>|| p' |||
      basepair <<< ('u', 'g') >>>|| p'
  
  k1 [p1,p2,k1,k2] = ([k1,p1],[p2,k2])
  k2 [p1,p2] = ([p1],[p2])
  
  k = tabulated2 $
      knot1 <<< p ~~~|| k >>>|| k1 |||
      knot2 <<< p >>>|| k2
      
  z = mk inp
  tabulated1 = table1 z
  tabulated2 = table2 z
  
  axiom' :: Array Int a -> RichParser a b -> [b]
  axiom' z (_,ax) =
      let (_,l) = bounds z
      in ax z [0,0,0,l]
  in axiom' z s