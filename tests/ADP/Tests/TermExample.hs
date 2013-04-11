module ADP.Tests.TermExample where

import ADP.Multi.All
import ADP.Multi.Rewriting.All
                                 
type Term_Algebra alphabet answer = (
  answer -> answer,
  answer -> answer,                              -- sym
  alphabet -> answer -> answer, -- sym1
  alphabet -> answer, -- sym2
  alphabet -> alphabet -> alphabet -> alphabet, -- escape
  answer   -> alphabet -> answer -> alphabet -> answer,               -- fun
  answer   -> answer,               -- single
  answer   -> alphabet -> answer -> answer               -- split
  )
   
prettyprint :: Term_Algebra Char String
prettyprint = (wrap,sym,sym1,sym2,escape,fun,single,split) where
   wrap s = s
   sym s = s
   sym1 c s = [c] ++ s
   sym2 c = [c]
   escape _ b _ = b
   fun i _ a _ = i ++ ['('] ++ a ++ [')']
   single s = s
   split s _ a = s ++ [','] ++ a 
   
tikztree :: Term_Algebra Char String
tikztree = (wrap,sym,sym1,sym2,escape,fun,single,split) where
   wrap s = "\\" ++ s ++ ";" 
   sym s = "node{" ++ s ++ "}"
   sym1 c s = [c] ++ s
   sym2 c = [c]
   escape _ b _ = b
   fun i _ a _ = i ++ a
   single s = "child{" ++ s ++ "}" 
   split s _ a = "child{" ++ s ++ "}" ++ a
   
qtree :: (String -> String) -- custom symbol formatting 
         -> Term_Algebra Char String
qtree format = (wrap,sym,sym1,sym2,escape,fun,single,split) where
   wrap s = "\\Tree " ++ s 
   sym s = format s
   sym1 c s = [c] ++ s
   sym2 c = [c]
   escape _ b _ = b
   fun i _ a _ = "[." ++ i ++ " " ++ a ++ " ]"
   single s = s 
   split s _ a = s ++ " " ++ a

   
term :: Term_Algebra Char answer -> String -> [answer]
term algebra inp =
  let  
  (wrap,sym,sym1,sym2,escape,fun,single,split) = algebra
  
  s'= wrap <<< s >>> id1
     
  s = tabulated $
      i |||
      fun <<< i ~~~ '(' ~~~ a ~~~ ')' >>> id1
    
  i = tabulated $
      sym <<< i' >>> id1
      
  i' = tabulated $
      yieldSize1 (1,Nothing) $
      sym1 <<< i1 ~~~ i' >>> id1 |||
      sym1 <<< i2 ~~~ i' >>> id1 |||
      sym2 <<< i1        >>> id1 |||
      sym2 <<< i2        >>> id1
      
  i1= tabulated $
      anycharExcept ['(', ')', ',']
      
  i2= tabulated $
      escape <<< '\'' ~~~ '(' ~~~ '\'' >>> id1 |||
      escape <<< '\'' ~~~ ')' ~~~ '\'' >>> id1 |||
      escape <<< '\'' ~~~ ',' ~~~ '\'' >>> id1 
  
  a = tabulated $
      yieldSize1 (1,Nothing) $
      single <<< s               >>> id1 |||
      split  <<< s ~~~ ',' ~~~ a >>> id1

  z = mk inp
  tabulated = table1 z
  
  in axiom z s'