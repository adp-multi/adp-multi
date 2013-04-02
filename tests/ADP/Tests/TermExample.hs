{-# LANGUAGE ImplicitParams #-}

module ADP.Tests.TermExample where

import ADP.Multi.SimpleParsers
import ADP.Multi.Combinators
import ADP.Multi.Tabulation
import ADP.Multi.Helpers
import ADP.Multi.Rewriting
                                 
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
  
  s'= wrap <<< s >>>| id
     
  s = tabulated $
      i |||
      fun <<< i ~~~ '(' ~~~| a ~~~ ')' >>>| id
    
  i = tabulated $
      sym <<< i' >>>| id
      
  i' = tabulated $
      sym1 <<< i1 ~~~| i' >>>| id |||
      sym1 <<< i2 ~~~| i' >>>| id |||
      sym2 <<< i1 >>>| id |||
      sym2 <<< i2 >>>| id
      
  i1= tabulated $
      anycharExcept ['(', ')', ',']
      
  i2= tabulated $
      escape <<< '\'' ~~~ '(' ~~~ '\'' >>>| id |||
      escape <<< '\'' ~~~ ')' ~~~ '\'' >>>| id |||
      escape <<< '\'' ~~~ ',' ~~~ '\'' >>>| id 
  
  a = tabulated $
      single <<< s >>>| id |||
      split <<< s ~~~ ',' ~~~| a >>>| id

  z = mk inp
  tabulated = table1 z
  
  in axiom z s'