-- | This example implements the 1-structure grammar from
--   "Topology and prediction of RNA pseudoknots" by Reidys et al., 2011
module ADP.Tests.OneStructureExample where

import Data.Array

import ADP.Multi.All
import ADP.Multi.Rewriting.All

-- our own Char which has an index attached
-- with that we can access the terminal index in algebras
data MyChar = MyChar Char Integer
instance Eq MyChar where
    (MyChar c _) == (MyChar c2 _) = c == c2
instance Show MyChar where
    show (MyChar c i) = show c
type MyString = [MyChar]
myString :: String -> MyString
myString s = map (\(c,i) -> MyChar c i) (zip s [0..])
toString = map (\(MyChar c i) -> c)
                          
type OneStructure_Algebra alphabet ans = (
  EPS -> ans,                        -- nil
  ans -> ans -> ans,                 -- left
  ans -> ans -> ans -> ans,          -- pair
  ([alphabet], [alphabet]) -> ans,       -- basepair
  [alphabet] -> ans,                   -- base
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

infixl ***
(***) :: (Eq b, Eq c) => OneStructure_Algebra a b -> OneStructure_Algebra a c -> OneStructure_Algebra a (b,c)
alg1 *** alg2 = (nil,left,pair,basepair,base,i1,i2,tstart,knotH,knotK,knotL,knotM
              ,aknot1,aknot2,bknot1,bknot2,cknot1,cknot2,dknot1,dknot2,h) where
   (nil',left',pair',basepair',base',i1',i2',tstart',knotH',knotK',knotL',knotM'
              ,aknot1',aknot2',bknot1',bknot2',cknot1',cknot2',dknot1',dknot2',h') = alg1
   (nil'',left'',pair'',basepair'',base'',i1'',i2'',tstart'',knotH'',knotK'',knotL'',knotM''
              ,aknot1'',aknot2'',bknot1'',bknot2'',cknot1'',cknot2'',dknot1'',dknot2'',h'') = alg2
   
   nil a = (nil' a, nil'' a)
   left (b1,b2) (s1,s2) = (left' b1 s1, left'' b2 s2)
   pair (p1,p2) (s11,s21) (s12,s22) = (pair' p1 s11 s12, pair'' p2 s21 s22)
   basepair a = (basepair' a,  basepair'' a)
   base a = (base' a, base'' a)
   i1 (s1,s2) = (i1' s1, i1'' s2)
   i2 (t1,t2) = (i2' t1, i2'' t2)
   tstart (p1,p2) (i1,i2) (t1,t2) (s1,s2) = (tstart' p1 i1 t1 s1, tstart'' p2 i2 t2 s2)
   knotH (s1,s2) (i11,i12) (i21,i22) (i31,i32) (i41,i42) (a1,a2) (b1,b2) =
     (knotH' s1 i11 i21 i31 i41 a1 b1, knotH'' s2 i12 i22 i32 i42 a2 b2)
   knotK (s1,s2) (i11,i12) (i21,i22) (i31,i32) (i41,i42) (i51,i52) (i61,i62) (a1,a2) (b1,b2) (c1,c2) =
     (knotK' s1 i11 i21 i31 i41 i51 i61 a1 b1 c1, knotK'' s2 i12 i22 i32 i42 i52 i62 a2 b2 c2)
   knotL (s1,s2) (i11,i12) (i21,i22) (i31,i32) (i41,i42) (i51,i52) (i61,i62) (a1,a2) (b1,b2) (c1,c2) =
     (knotL' s1 i11 i21 i31 i41 i51 i61 a1 b1 c1, knotL'' s2 i12 i22 i32 i42 i52 i62 a2 b2 c2)
   knotM (s1,s2) (i11,i12) (i21,i22) (i31,i32) (i41,i42) (i51,i52) (i61,i62) (i71,i72) (i81,i82) (a1,a2) (b1,b2) (c1,c2) (d1,d2) = 
     (knotM' s1 i11 i21 i31 i41 i51 i61 i71 i81 a1 b1 c1 d1, knotM'' s2 i12 i22 i32 i42 i52 i62 i72 i82 a2 b2 c2 d2)
   aknot1 = xknot1 aknot1' aknot1''
   bknot1 = xknot1 bknot1' bknot1''
   cknot1 = xknot1 cknot1' cknot1''
   dknot1 = xknot1 dknot1' dknot1''
   aknot2 = xknot2 aknot2' aknot2''
   bknot2 = xknot2 bknot2' bknot2''
   cknot2 = xknot2 cknot2' cknot2''
   dknot2 = xknot2 dknot2' dknot2''
   xknot1 xknot1' xknot1'' (p1,p2) (i11,i12) (i21,i22) (x1,x2) = 
     (xknot1' p1 i11 i21 x1, xknot1'' p2 i12 i22 x2)
   xknot2 xknot2' xknot2'' (p1,p2) = (xknot2' p1, xknot2'' p2)

   h xs = [ (x1,x2) |
            x1 <- h'  [ y1 | (y1,_)  <- xs]
          , x2 <- h'' [ y2 | (y1,y2) <- xs, y1 == x1]
          ]

data T = Nil
       | Left' T T
       | Pair T T T
       | BasePair (MyString, MyString)
       | Base MyString
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

enum :: OneStructure_Algebra MyChar T
enum = (\_->Nil,Left',Pair,BasePair,Base,I1,I2,TStart,KnotH,KnotK,KnotL,KnotM
       ,XKnot1,XKnot2,XKnot1,XKnot2,XKnot1,XKnot2,XKnot1,XKnot2,id)
   
-- | dot-bracket
prettyprint :: OneStructure_Algebra MyChar [String]
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
   bknot2 _ = [ "[" , "]" ]
   cknot1 _ = xknot1 "{" "}"
   cknot2 _ = [ "{" , "}" ]
   dknot1 _ = xknot1 "<" ">"
   dknot2 _ = [ "<" , ">" ]
   
   xknot1 parenL parenR i1 i2 [x1,x2] = 
        [concat $ [parenL] ++ i1 ++ [x1], concat $ [x2] ++ i2 ++ [parenR]]
      
   h = id
   
-- | reconstructed input
prettyprint2 :: OneStructure_Algebra MyChar [String]
prettyprint2 = (nil,left,pair,basepair,base,i1,i2,tstart,knotH,knotK,knotL,knotM
              ,aknot1,aknot2,bknot1,bknot2,cknot1,cknot2,dknot1,dknot2,h) where
   nil _ = [""]
   left b s = [concat $ b ++ s]
   pair [p1,p2] s1 s2 = [concat $ [p1] ++ s1 ++ [p2] ++ s2]
   basepair ([MyChar b1 _], [MyChar b2 _]) = [[b1],[b2]]
   base ([MyChar b _]) = [[b]]
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

-- | base pair maximization
bpmax :: OneStructure_Algebra MyChar Int
bpmax = (nil,left,pair,basepair,base,i1,i2,tstart,knotH,knotK,knotL,knotM
              ,aknot1,aknot2,bknot1,bknot2,cknot1,cknot2,dknot1,dknot2,h) where
   nil _ = 0
   left b s = b + s
   pair p s1 s2 = p + s1 + s2
   basepair _ = 1
   base _ = 0
   i1 s = s
   i2 t = t
   tstart p i t s = p + i + t + s
   knotH s i1 i2 i3 i4 a b = s + i1 + i2 + i3 + i4 + a + b
   knotK s i1 i2 i3 i4 i5 i6 a b c = s + i1 + i2 + i3 + i4 + i5 + i6 + a + b + c
   knotL s i1 i2 i3 i4 i5 i6 a b c = s + i1 + i2 + i3 + i4 + i5 + i6 + a + b + c
   knotM s i1 i2 i3 i4 i5 i6 i7 i8 a b c d = s + i1 + i2 + i3 + i4 + i5 + i6 + i7 + i8 + a + b + c + d
   aknot1 = xknot1
   aknot2 p = p
   bknot1 = xknot1
   bknot2 p = p
   cknot1 = xknot1
   cknot2 p = p
   dknot1 = xknot1
   dknot2 p = p
   
   xknot1 p i1 i2 x = p + i1 + i2 + x
      
   h [] = [] 
   h xs = [maximum xs]
   
-- see ABABExample.hs
texforestnew :: OneStructure_Algebra MyChar String
texforestnew = (nil,left,pair,basepair,base,i1,i2,tstart,knotH,knotK,knotL,knotM
              ,aknot1,aknot2,bknot1,bknot2,cknot1,cknot2,dknot1,dknot2,h) where
   term c idx = "[" ++ c ++ ", leaf position=" ++ show idx ++ "]"

   -- a small hack
   nil (EPS i) = "[S [$\\epsilon$, leaf position=" ++ show ((fromIntegral i)-0.5) ++ " ] ] "
   left b s = "[S " ++ b ++ s ++ "]"
   pair p s1 s2 = "[S " ++ p ++ s1 ++ s2 ++ "]"
   basepair ([MyChar b1 i1],[MyChar b2 i2]) = "[P " ++ term [b1] i1 ++ term [b2] i2 ++ "] "
   base [MyChar b i] = "[U " ++ term [b] i ++ "] "
   i1 s = "[I " ++ s ++ "]"
   i2 t = "[I " ++ t ++ "]"
   -- Note: The node order is different to match the productions of the paper.
   tstart p i t s = "[T " ++ i ++ p ++ t ++ s ++ "]"
   knotH s i1 i2 i3 i4 a b = "[T " ++ i1 ++ a ++ i2 ++ b ++ i3 ++ i4 ++ s ++ "]"
   knotK s i1 i2 i3 i4 i5 i6 a b c = "[T " ++ i1 ++ a ++ i2 ++ b ++ i3 ++ i4 ++ c ++ i5 ++ i6 ++ s ++ "]"
   knotL s i1 i2 i3 i4 i5 i6 a b c = "[T " ++ i1 ++ a ++ i2 ++ b ++ i3 ++ c ++ i4 ++ i5 ++ i6 ++ s ++ "]"
   knotM s i1 i2 i3 i4 i5 i6 i7 i8 a b c d = "[T " ++ i1 ++ a ++ i2 ++ b ++ i3 ++ c ++ i4 ++ i5 ++ d ++ i6 ++ i7 ++ i8 ++ s ++ "]"
   aknot1 = xknot1 "A"
   aknot2 p = "[A " ++ p ++ "]"
   bknot1 = xknot1 "B"
   bknot2 p = "[B " ++ p ++ "]"
   cknot1 = xknot1 "C"
   cknot2 p = "[C " ++ p ++ "]"
   dknot1 = xknot1 "D"
   dknot2 p = "[D " ++ p ++ "]"
   
   xknot1 nt p i1 i2 x = "[" ++ nt ++ " " ++ p ++ i1 ++ x ++ i2 ++ "]"
   
   h = id
     
   

{- To make the grammar reusable, its definition has been split up into the
   actual grammar which exposes the start symbol as a parser (oneStructureGrammar)
   and a convenience function which actually runs the grammar on a given input (oneStructure).
   It is reused in ZeroStructureTwoBackbonesExample.hs
-}
oneStructure :: OneStructure_Algebra MyChar ans -> String -> [ans]
oneStructure algebra inp =
    let z = mk (myString inp)
        grammar = oneStructureGrammar algebra z
    in axiom z grammar

oneStructureGrammar :: OneStructure_Algebra MyChar ans -> Array Int MyChar -> RichParser MyChar ans
oneStructureGrammar algebra z =
  let  
  (nil,left,pair,basepair,base,i1,i2,tstart,knotH,knotK,knotL,knotM,
   aknot1,aknot2,bknot1,bknot2,cknot1,cknot2,dknot1,dknot2,h) = algebra
   
  i = tabulated1 $
      i1 <<< s >>> id1 |||
      i2 <<< t >>> id1
      ... h
  
  rewritePair, rewriteTStart, rewriteKnotH, rewriteKnotK, rewriteKnotL, rewriteKnotM :: Dim1
  
  rewritePair [p1,p2,s1,s2] = [p1,s1,p2,s2]
  
  s = tabulated1 $
      yieldSize1 (0, Nothing) $
      nil  <<< EPS 0 >>> id1 |||
      left <<< b ~~~ s >>> id1 |||
      pair <<< p ~~~ s ~~~ s >>> rewritePair
      ... h
      
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
      ... h
      
  rewriteXKnot1 :: Dim2      
  rewriteXKnot1 [p1,p2,i1,i2,x1,x2] = ([p1,i1,x1],[x2,i2,p2])
  
  xa = tabulated2 $
      yieldSize2 (1, Nothing) (1, Nothing) $
      aknot1 <<< p ~~~ i ~~~ i ~~~ xa >>> rewriteXKnot1 |||
      aknot2 <<< p >>> id2
      ... h
      
  xb = tabulated2 $
      yieldSize2 (1, Nothing) (1, Nothing) $
      bknot1 <<< p ~~~ i ~~~ i ~~~ xb >>> rewriteXKnot1 |||
      bknot2 <<< p >>> id2
      ... h
      
  xc = tabulated2 $
      yieldSize2 (1, Nothing) (1, Nothing) $
      cknot1 <<< p ~~~ i ~~~ i ~~~ xc >>> rewriteXKnot1 |||
      cknot2 <<< p >>> id2
      ... h
      
  xd = tabulated2 $
      yieldSize2 (1, Nothing) (1, Nothing) $
      dknot1 <<< p ~~~ i ~~~ i ~~~ xd >>> rewriteXKnot1 |||
      dknot2 <<< p >>> id2
      ... h
  
  p = tabulated2 $
      basepair <<< (myString "a", myString "u") >>> id2 |||
      basepair <<< (myString "u", myString "a") >>> id2 |||
      basepair <<< (myString "c", myString "g") >>> id2 |||
      basepair <<< (myString "g", myString "c") >>> id2 |||
      basepair <<< (myString "g", myString "u") >>> id2 |||
      basepair <<< (myString "u", myString "g") >>> id2

  b = tabulated1 $
      base <<< myString "a" >>> id1 |||
      base <<< myString "u" >>> id1 |||
      base <<< myString "c" >>> id1 |||
      base <<< myString "g" >>> id1
       
  tabulated1 = table1 z
  tabulated2 = table2 z
  
  in i
