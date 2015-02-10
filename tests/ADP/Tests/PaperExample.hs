module ADP.Tests.PaperExample where

import ADP.Multi.All
import ADP.Multi.Rewriting.All 
                 
              
type Paper_Algebra alphabet answer = (
  answer   -> answer -> answer -> answer, -- fz1
  answer   -> answer -> answer,           -- fz2
  answer   -> answer -> answer -> answer, -- fz3
  [alphabet] -> answer,                   -- fe
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

enum :: Paper_Algebra Char Term
enum = (FZ1,FZ2,FZ3,\_->FE,FK1,FK2,FL1,FL2,FP,FB,id)

bpmax :: Paper_Algebra Char Int
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

dotbracket :: Paper_Algebra Char [String]
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
   
grammar :: Paper_Algebra Char answer -> String -> [answer]
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
      fe <<< "" >>> id1     
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
      fp <<< ("a", "u") >>> id2 |||
      fp <<< ("u", "a") >>> id2 |||
      fp <<< ("c", "g") >>> id2 |||
      fp <<< ("g", "c") >>> id2 |||
      fp <<< ("g", "u") >>> id2 |||
      fp <<< ("u", "g") >>> id2

  b = tabulated1 $
      fb <<< "a" >>> id1 |||
      fb <<< "u" >>> id1 |||
      fb <<< "c" >>> id1 |||
      fb <<< "g" >>> id1

  inpa = mk inp
  tabulated1 = table1 inpa
  tabulated2 = table2 inpa
  
  in axiom inpa z
