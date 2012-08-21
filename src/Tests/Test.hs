module Tests.Test where

import Data.Char (ord)
import Data.Array (bounds, (!))
import ADP.Multi.SimpleParsers
import ADP.Multi.Combinators
import ADP.Multi.Tabulation
import ADP.Multi.Helpers
import ADP.Multi.Rewriting.Simple()

-- # TESTS, see RGExample for a real test

type TestAlgebra alphabet answer = (
        () -> answer,
        (EPS, alphabet) -> answer,
        (alphabet, alphabet) -> answer,
        (alphabet, alphabet) -> (alphabet, alphabet) -> answer,
        answer -> answer -> answer,
        answer -> answer
   )

testAlg :: TestAlgebra Char Int
testAlg = (nil, left, f, f2, f3, start) where
   nil () = 0
   left (EPS,c) = ord c
   f (l,r) = ord l + ord r
   f2 (l,r) (l2,r2) = ord l + ord r + ord l2 + ord r2
   f3 l r = l + r
   start r = r
   
testGram :: TestAlgebra Char answer -> String -> [answer]
testGram alg inp = axiom s where
  (nil, left, f, f2, f3, start) = alg
  
  s',f3',f' :: [(Int,Int)] -> ([(Int,Int)],[(Int,Int)]) 
  
  s' [c1,c2] = ([],[c1,c2]) -- 1-dim simulated as 2-dim
  
  s = start <<< k >>> s'

  
  f3' [p1,p2,k1,k2] = ([k1,p1],[p2,k2])

  k = f3 <<< p ~~~| k >>> f3' ||| 
      start <<< p >>> f' -- or just p
  

  f' [c1,c2] = ([c1],[c2]) -- "identical" function
  p = --tabulated (
      f <<< ('a', 'u') >>> f' |||
      f <<< ('u', 'a') >>> f' |||
      f <<< ('c', 'g') >>> f' |||
      f <<< ('g', 'c') >>> f' |||
      f <<< ('g', 'u') >>> f' |||
      f <<< ('u', 'g') >>> f' 
      --)
      
  test = f   <<< ('a', 'u') >>> f' |||
         nil <<< ()         >>> f' |||
         left <<< (EPS, 'u') >>> f'
      
  -- or using our basepair parser constructed with a filter:
  
  p2 = f <<< basepair >>> f'

  z         = mk inp
  (_,n)     = bounds z
  
  tabulated = table2 n

  basepair  = anychars `with2` basepairing 
  basepairing :: Filter2 Char
  basepairing z (i,j,k,l) = i+1 == j && k+1 == l && isBasepair (z!(i+1), z!(k+1))
  
  axiom     = axiom' n z
  
isBasepair ('a','u') = True
isBasepair ('u','a') = True
isBasepair ('c','g') = True
isBasepair ('g','c') = True
isBasepair ('g','u') = True
isBasepair ('u','g') = True
isBasepair _         = False
  
test = testGram testAlg "guaugc"
