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
             Left' Char Start           |
             Pair Start Start Start     |
             Knot Start Start Start Start Start Start |
             Knot1 Start Start          |
             Knot2 Start                |
             BasePair Char Char             
                                   deriving (Eq, Show)
                                   
type RG_Algebra alphabet answer = (
  () -> answer,                               -- nil
  alphabet -> answer -> answer,               -- left
  answer   -> answer -> answer -> answer,     -- pair
  answer   -> answer -> answer -> answer -> answer -> answer -> answer, -- knot
  answer   -> answer -> answer,               -- knot1
  answer   -> answer,                         -- knot2
  alphabet -> alphabet -> answer,             -- basepair
  [answer] -> [answer]                        -- h
  )
  
enum :: RG_Algebra Char Start
enum = (nil,left,pair,knot,knot1,knot2,basepair,h) where
   nil _ = Nil
   left  = Left'
   pair  = Pair
   knot  = Knot
   knot1 = Knot1
   knot2 = Knot2
   basepair = BasePair
   h     = id

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
