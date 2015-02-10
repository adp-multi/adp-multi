module ADP.Tests.PaperExample where

import ADP.Multi.All
import ADP.Multi.Rewriting.All 
import ADP.Tests.ABABExample(MyChar(..),myString,toString)
              
type Paper_Algebra alphabet answer = (
  answer   -> answer -> answer -> answer, -- fz1
  answer   -> answer -> answer,           -- fz2
  answer   -> answer -> answer -> answer, -- fz3
  EPS -> answer,                          -- fe
  answer   -> answer -> answer,           -- fk1
  answer   -> answer,                     -- fk2
  answer   -> answer -> answer,           -- fl1
  answer   -> answer,                     -- fl2
  ([alphabet], [alphabet]) -> answer,     -- fp
  [alphabet] -> answer,                   -- fb
  [answer] -> [answer]                    -- h
  )
  
data Term = FZ1 Term Term Term
          | FZ2 Term Term
          | FZ3 Term Term Term
          | FE
          | FK1 Term Term
          | FK2 Term
          | FL1 Term Term
          | FL2 Term
          | FP (String, String)
          | FB String
          deriving (Eq, Show)

enum :: Paper_Algebra MyChar Term
enum = (FZ1,FZ2,FZ3,\_->FE,FK1,FK2,FL1,FL2,
        \(p1,p2) -> FP (toString p1,toString p2),
        \b -> FB (toString b),
        id)

bpmax :: Paper_Algebra MyChar Int
bpmax = (fz1,fz2,fz3,fe,fk1,fk2,fl1,fl2,fp,fb,h) where
   fz1 k l z   = k + l + z
   fz2 b z     = z
   fz3 p z1 z2 = p + z1 + z2
   fe _        = 0
   fk1 p k     = p + k
   fk2 p       = p
   fl1 p l     = p + l
   fl2 p       = p
   fp _        = 1
   fb _        = 0
   h [] = []
   h xs = [maximum xs]

dotbracket :: Paper_Algebra MyChar [String]
dotbracket = (fz1,fz2,fz3,fe,fk1,fk2,fl1,fl2,fp,fb,h) where
   fz1 [k1,k2] [l1,l2] [z] = [k1 ++ l1 ++ k2 ++ l2 ++ z]
   fz2 [b] [z]             = [b ++ z]
   fz3 [p1,p2] [z1] [z2]   = [p1 ++ z1 ++ p2 ++ z2]
   fe _                    = [""]
   fk1 _ [k1,k2]           = [k1 ++ "(",")" ++ k2]
   fk2 _                   = ["(",")"]
   fl1 _ [l1,l2]           = [l1 ++ "[","]" ++ l2]
   fl2 _                   = ["[","]"]
   fp _                    = ["(",")"]
   fb _                    = ["."]
   h = id

-- see ABABExample.hs
texforestnew :: Paper_Algebra MyChar String
texforestnew = (fz1,fz2,fz3,fe,fk1,fk2,fl1,fl2,fp,fb,h) where
   term c idx = "[" ++ c ++ ", leaf position=" ++ show idx ++ "] "
   fz1 k l z = "[Z " ++ k ++ l ++ z ++ " ] "
   fz2 b z = "[Z " ++ b ++ z ++ "] "
   fz3 p z1 z2 = "[Z " ++ p ++ z1 ++ z2 ++ "] "
   -- a small hack
   fe (EPS i) = "[Z [$\\epsilon$, leaf position=" ++ show ((fromIntegral i)-0.5) ++ " ] ] "
   fk1 p k = "[K " ++ p ++ k ++ "]"
   fk2 p = "[K " ++ p ++ "]"
   fl1 p l = "[L " ++ p ++ l ++ "]"
   fl2 p = "[L " ++ p ++ "]"
   fp ([MyChar b1 i1],[MyChar b2 i2]) = "[P " ++ term [b1] i1 ++ term [b2] i2 ++ "] "
   fb [MyChar b i] = "[B " ++ term [b] i ++ "] "
   h = id
   
grammar :: Paper_Algebra MyChar answer -> String -> [answer]
grammar algebra inp =
  let  
  (fz1,fz2,fz3,fe,fk1,fk2,fl1,fl2,fp,fb,h) = algebra
   
  rz1, rz3 :: Dim1 
   
  rz1 [k1,k2,l1,l2,z] = [k1,l1,k2,l2,z]
  rz3 [p1,p2,z1,z2] = [p1,z1,p2,z2]
  
  z = tabulated1 $
      yieldSize1 (0,Nothing) $
      fz1 <<< k ~~~ l ~~~ z >>> rz1 |||
      fz2 <<< b ~~~ z >>> id1 |||
      fz3 <<< p ~~~ z ~~~ z >>> rz3 |||
      fe <<< EPS 0 >>> id1     
      ... h
  
  rk1 :: Dim2
  rk1 [p1,p2,k1,k2] = ([k1,p1],[p2,k2])
  
  k = tabulated2 $
      yieldSize2 (1,Nothing) (1,Nothing) $
      fk1 <<< p ~~~ k >>> rk1 |||
      fk2 <<< p >>> id2
      
  l = tabulated2 $
      yieldSize2 (1,Nothing) (1,Nothing) $
      fl1 <<< p ~~~ l >>> rk1 |||
      fl2 <<< p >>> id2
  
  p = tabulated2 $
      fp <<< (myString "a", myString "u") >>> id2 |||
      fp <<< (myString "u", myString "a") >>> id2 |||
      fp <<< (myString "c", myString "g") >>> id2 |||
      fp <<< (myString "g", myString "c") >>> id2 |||
      fp <<< (myString "g", myString "u") >>> id2 |||
      fp <<< (myString "u", myString "g") >>> id2

  b = tabulated1 $
      fb <<< myString "a" >>> id1 |||
      fb <<< myString "u" >>> id1 |||
      fb <<< myString "c" >>> id1 |||
      fb <<< myString "g" >>> id1

  inpa = mk (myString inp)
  tabulated1 = table1 inpa
  tabulated2 = table2 inpa
  
  in axiom inpa z
