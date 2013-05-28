-- | Alignment of trees / terms (Jiang et al., 1995)
module ADP.Tests.TreeAlignExample where

{-
In ADP-MCFL notation:

X -> (rep,r0)(L,L,X) |
     (del,r1)(L,X)   |
     (ins,r2)(L,X)   |
     (mty,r3)()      |
     (concat,r4)(X,X)
L -> f | g

r0(l1,l2,(x1,x2))   = (l1(x1),l2(x2))
r1(l,(x1,x2))       = (l(x1),x2)
r2(l,(x1,x2))       = (x1,l(x2))
r3()                = (,)
r4((x1,x2),(x3,x4)) = ( x1,x3 , x2,x4 )

In adp-multi, terminals in rewriting functions (here parentheses)
are moved to the productions.
-}

import ADP.Multi.All
import ADP.Multi.Rewriting.All
                 
           
type TreeAlign_Algebra alphabet answer = (
  alphabet -> alphabet -> answer -> alphabet -> alphabet -> alphabet -> alphabet -> answer,   -- rep
  alphabet -> answer -> alphabet -> alphabet -> answer,                                       -- del
  alphabet -> answer -> alphabet -> alphabet -> answer,                                       -- ins
  (EPS,EPS) -> answer,                                                                        -- mty
  answer -> answer -> alphabet -> alphabet -> answer,                                         -- concat
  [answer] -> [answer]                                                                        -- h
  )
  
infixl ***
(***) :: (Eq b, Eq c) => TreeAlign_Algebra a b -> TreeAlign_Algebra a c -> TreeAlign_Algebra a (b,c)
alg1 *** alg2 = (rep,del,ins,mty,concat,h) where
   (rep',del',ins',mty',concat',h') = alg1
   (rep'',del'',ins'',mty'',concat'',h'') = alg2
   
   rep l1 l2 (x1,x2) po1 pc1 po2 pc2 = (rep' l1 l2 x1 po1 pc1 po2 pc2, rep'' l1 l2 x2 po1 pc1 po2 pc2)
   del l (x1,x2) po pc = (del' l x1 po pc, del'' l x2 po pc)
   ins l (x1,x2) po pc = (ins' l x1 po pc, ins'' l x2 po pc)
   mty e = (mty' e, mty'' e)
   concat (x1,x2) (x3,x4) c1 c2 = (concat' x1 x3 c1 c2, concat'' x2 x4 c1 c2)
   h xs = [ (x1,x2) |
            x1 <- h'  [ y1 | (y1,_)  <- xs]
          , x2 <- h'' [ y2 | (y1,y2) <- xs, y1 == x1]
          ]
  
data Term = Rep Char Char Term
          | Del Char Term
          | Ins Char Term
          | Mty
          | Concat Term Term
          deriving (Eq, Show)
          
term :: TreeAlign_Algebra Char Term
term = (rep,del,ins,mty,concat,h) where
   rep l1 l2 x _ _ _ _  = Rep l1 l2 x
   del l x _ _          = Del l x 
   ins l x _ _          = Ins l x
   mty _                = Mty
   concat x1 x2 _ _     = Concat x1 x2
   h                    = id

treeSimilarity :: TreeAlign_Algebra Char Int
treeSimilarity = (rep,del,ins,mty,concat,h) where
   rep l1 l2 x _ _ _ _  = x + (if l1 == l2 then 1 else 0)
   del _ x _ _          = x - 1
   ins _ x _ _          = x - 1
   mty _                = 0
   concat x1 x2 _ _     = x1 + x2
   h []                 = []
   h xs                 = [maximum xs]

treeAlign :: TreeAlign_Algebra Char answer -> (String,String) -> [answer]
treeAlign algebra (inp1,inp2) =
  let  
  (rep,del,ins,mty,concat,h) = algebra
   
  rRep, rDel, rIns, rConcat :: Dim2
  
  rRep [l1,l2,x1,x2,po1,pc1,po2,pc2] = ([l1,po1,x1,pc1],[l2,po2,x2,pc2])
  rDel [l,x1,x2,po,pc] = ([l,po,x1,pc],[x2])
  rIns [l,x1,x2,po,pc] = ([x1],[l,po,x2,pc])
  rConcat [x1,x2,x3,x4,c1,c2] = ([x1,c1,x3],[x2,c2,x4])
  
  x = tabulated2 $
      yieldSize2 (0,Nothing) (0,Nothing) $
      rep    <<< l ~~~ l ~~~ x ~~~ '(' ~~~ ')' ~~~ '(' ~~~ ')' >>> rRep |||
      del    <<< l ~~~ x ~~~ '(' ~~~ ')'                       >>> rDel |||
      ins    <<< l ~~~ x ~~~ '(' ~~~ ')'                       >>> rIns |||
      mty    <<< (EPS,EPS)                                     >>> id2  |||
      concat <<< x ~~~ x ~~~ ',' ~~~ ','                       >>> rConcat
      ... h
  
  l = char 'f' |||
      char 'g'
      
  z = mkTwoTrack inp1 inp2
  tabulated2 = table2 z
  
  in axiomTwoTrack z inp1 inp2 x
  
test = treeAlign (treeSimilarity *** term) ("f(f(),g(f()))","f(f(),g(f()))")
test2 = treeAlign (treeSimilarity *** term) ("f(f(),g())","f(f(),g(f()))")