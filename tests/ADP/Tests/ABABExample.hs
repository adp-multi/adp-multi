-- | ABAB language L = a^i b^j a^i b^j
module ADP.Tests.ABABExample where

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

type ABAB_Algebra alphabet answerDim1 answerDim2 = (
  answerDim2 -> answerDim2 -> answerDim1,          -- fz
  answerDim2 -> ([alphabet],[alphabet]) -> answerDim2, -- fp
  (EPS,EPS)  -> answerDim2,                        -- nil
  [answerDim1] -> [answerDim1],                     -- h
  [answerDim2] -> [answerDim2]                     -- h2
  )

data Start =
    Nil
  | Fz Start Start
  | Fp Start ([MyChar],[MyChar])
  deriving (Eq, Show)

enum :: ABAB_Algebra MyChar Start Start
enum = (Fz,Fp,\_ -> Nil,id,id)

count :: ABAB_Algebra MyChar Int Int
count = (fz,fp,nil,h,h2) where
   fz a b = a
   fp a _ = a
   nil _  = 1
   h [] = [0]
   h m = m --[sum m]
   h2 = h

prettyprint :: ABAB_Algebra MyChar String (String,String)
prettyprint = (fz,fp,nil,h,h2) where
   fz (a,a') (b,b') = a ++ b ++ a' ++ b'
   fp (a,a') ([MyChar 'a' _],_) = (a ++ "[", a' ++ "]")
   fp (a,a') ([MyChar 'b' _],_) = (a ++ "(", a' ++ ")")
   nil _ = ("","")
   h = id
   h2 = id
   
wordproblem :: ABAB_Algebra MyChar Bool Bool
wordproblem = (fz,fp,nil,h,h2) where
   fz a b = True
   fp a _ = True
   nil _  = True
   h [] = [False]
   h m = [or m]
   h2 = h

texforest :: ABAB_Algebra MyChar String String
texforest = (fz,fp,nil,h,h2) where
   term c idx = "[" ++ c ++ ", before drawing tree={x=\\xleaf{" ++ show idx ++ "}}] "
   fz a b = "[Z " ++ a ++ b ++ " ]"
   fp a ([MyChar 'a' i],[MyChar 'a' i2]) = "[A " ++ a ++ term "a" i ++ term "a" i2 ++ "] "
   fp a ([MyChar 'b' i],[MyChar 'b' i2]) = "[B " ++ a ++ term "b" i ++ term "b" i2 ++ "] "
   nil _ = ""
   h = id
   h2 = h
   
grammar :: ABAB_Algebra MyChar answerDim1 answerDim2 -> String -> [answerDim1]
grammar algebra inp =
  let
  (fz,fp,nil,h,h2) = algebra
  
  fzRewrite :: Dim1
  fzRewrite [a,a',b,b'] = [a,b,a',b']
  
  s = tabulated1 $
      fz <<< a ~~~ b >>> fzRewrite
      ... h 
  
  fpRewrite :: Dim2
  fpRewrite [a,a',c,d] = ([a,c],[a',d])
  
  a = tabulated2 $
      yieldSize2 (0,Nothing) (0,Nothing) $
      fp  <<< a ~~~ (myString "a",myString "a") >>> fpRewrite |||
      nil <<< (EPS,EPS)       >>> id2
      ... h2
  
  b = tabulated2 $
      yieldSize2 (0,Nothing) (0,Nothing) $
      fp  <<< b ~~~ (myString "b",myString "b") >>> fpRewrite |||
      nil <<< (EPS,EPS)       >>> id2
      ... h2
      
  z = mk (myString inp)
  tabulated1 = table1 z
  tabulated2 = table2 z
  
  in axiom z s
  