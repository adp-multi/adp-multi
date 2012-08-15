module ADP.Multi.RGExample where

{-
S -> € | BS | P_1 S P_2 S | K_1^1 S K_1^2 S K_2^1 S K_2^2 S
[K_1,K_2] -> [K_1 P_1, P_2 K_2] | [P_1, P_2]
[P_1,P_2] -> [a,u] | [u,a] | [g,c] | [c,g] | [g,u] | [u,g]
B -> a | u | c | g
-}

import Data.Array
import Data.List
import ADP.Multi.Combinators

data Start = Nil                        |
             Left' Start Start          |
             Pair Start Start Start     |
             Knot Start Start Start Start Start Start |
             Knot1 Start Start          |
             Knot2 Start                |
             BasePair (Char, Char)      |
             Base (EPS, Char)
                                   deriving (Eq, Show)
                                   
type RG_Algebra alphabet answer = (
  () -> answer,                               -- nil
  answer   -> answer -> answer,               -- left
  answer   -> answer -> answer -> answer,     -- pair
  answer   -> answer -> answer -> answer -> answer -> answer -> answer, -- knot
  answer   -> answer -> answer,               -- knot1
  answer   -> answer,                         -- knot2
  (alphabet, alphabet) -> answer,             -- basepair
  (EPS, alphabet) -> answer,                  -- base
  [answer] -> [answer]                        -- h
  )
  
enum :: RG_Algebra Char Start
enum = (nil,left,pair,knot,knot1,knot2,basepair,base,h) where
   nil _ = Nil
   left  = Left'
   pair  = Pair
   knot  = Knot
   knot1 = Knot1
   knot2 = Knot2
   basepair = BasePair
   base  = Base
   h     = id
   
rgknot :: RG_Algebra Char answer -> String -> [answer]
rgknot alg inp = axiom s where
  (nil,left,pair,knot,knot1,knot2,basepair,base,h) = alg
  
  s1,s2,s3,s4,p',k1,k2 :: [(Int,Int)] -> ([(Int,Int)],[(Int,Int)])
  
  -- all s are 1-dim simulated as 2-dim
  s1 [c1,c2] = ([],[c1,c2])
  s2 [b1,b2,s1,s2] = ([],[b1,b2,s1,s2])  
  s3 [p1,p2,s11,s12,s21,s22] = ([],[p1,s11,s12,p2,s21,s22])
  s4 [k11,k12,k21,k22,s11,s12,s21,s22,s31,s32,s41,s42] = ([],[k11,s11,s12,k21,s21,s22,k12,s31,s32,k22,s41,s42])
  
  s = nil <<< () >>> s1 |||
      left <<< b ~~~| s >>> s2 |||
      pair <<< p ~~~| s ~~~| s >>> s3 |||
      knot <<< k ~~~ k ~~~| s ~~~| s ~~~| s ~~~| s >>> s4
      
  b = base <<< (EPS, 'a') >>> s1 |||
      base <<< (EPS, 'u') >>> s1 |||
      base <<< (EPS, 'c') >>> s1 |||
      base <<< (EPS, 'g') >>> s1
  
  p' [c1,c2] = ([c1],[c2])
  p = basepair <<< ('a', 'u') >>> p' |||
      basepair <<< ('u', 'a') >>> p' |||
      basepair <<< ('c', 'g') >>> p' |||
      basepair <<< ('g', 'c') >>> p' |||
      basepair <<< ('g', 'u') >>> p' |||
      basepair <<< ('u', 'g') >>> p'
  
  k1 [p1,p2,k1,k2] = ([k1,p1],[p2,k2])
  k2 [p1,p2] = ([p1],[p2])
  
  k = knot1 <<< p ~~~| k >>> k1 |||
      knot2 <<< p >>> k2
      
  
  z         = mk inp
  (_,n)     = bounds (z)  
  tabulated = table2 n  
  axiom     = axiom' n z
  

{-
rgknot alg inp = axiom s where
  (nil,left,pair,knot,knot1,knot2,basepair,h) = alg

  s = tabulated (
        nil <<< empty |||
        left <<< base ~~~ s |||
        pair <<< p 1 1 ~~~ s ~~~ p 1 2 ~~~ s |||
        knot <<< k 1 1 ~~~ s ~~~ k 2 1 ~~~ s ~~~ k 1 2 ~~~ s ~~~ k 2 2 ~~~ s
        ... h
      )

  p upper lower =  (
        basepair <<<2 (base, base) -- TODO filter fehlt 
      )
  
  k upper lower =  (
        knot2 <<<2 (p 0 1, p 0 2) |||
        knot1 <<<2 (k 0 1 ~~~ p 0 1, p 0 2 ~~~ k 0 2)
      )

  z         = mk inp
  (_,n)     = bounds z
  base      = achar' z
  tabulated = table n
--  tabulated2 = table2 n
  axiom     = axiom' n
-}