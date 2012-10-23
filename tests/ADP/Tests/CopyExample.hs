{-# LANGUAGE ImplicitParams #-}

-- Copy language L = { ww | w € {a,b}^* }
module ADP.Tests.CopyExample where

import ADP.Multi.SimpleParsers
import ADP.Multi.Combinators
import ADP.Multi.Tabulation
import ADP.Multi.Helpers
import ADP.Multi.Rewriting
                                 
type Copy_Algebra alphabet answerDim1 answerDim2 = (
  (EPS,EPS)  -> answerDim2,                         -- nil
  answerDim2 -> answerDim1,                         -- copy
  alphabet -> alphabet -> answerDim2 -> answerDim2  -- copy'
  )

data Start = Nil
           | Copy Start
           | Copy' Char Char Start
           deriving (Eq, Show)

-- without consistency checks
enum :: Copy_Algebra Char Start Start
enum = (nil,copy,copy') where
   nil _ = Nil
   copy  = Copy
   copy' = Copy'
   
prettyprint :: Copy_Algebra Char String (String,String)
prettyprint = (nil,copy,copy') where
   copy (l,r) = l ++ r
   nil _ = ("","")   
   copy' c1 c2 (l,r) = (c1:l,c2:r)

-- (count of a's, count of b's)
countABs :: Copy_Algebra Char (Int,Int) (Int,Int)
countABs = (nil,copy,copy') where
   nil _                 = (0,0)
   copy (c1,c2)          = (c1*2,c2*2)
   copy' 'a' 'a' (c1,c2) = (c1+1,c2)
   copy' 'b' 'b' (c1,c2) = (c1,c2+1)
  
   
copyGr :: YieldAnalysisAlgorithm Dim1 -> RangeConstructionAlgorithm Dim1
       -> YieldAnalysisAlgorithm Dim2 -> RangeConstructionAlgorithm Dim2 
       -> Copy_Algebra Char answerDim1 answerDim2 -> String -> [answerDim1]
copyGr yieldAlg1 rangeAlg1 yieldAlg2 rangeAlg2 algebra inp =
  -- These implicit parameters are used by >>>.
  -- They were introduced to allow for exchanging the algorithms and
  -- they were made implicit so that they don't ruin our nice syntax.
  let ?yieldAlg1 = yieldAlg1
      ?rangeAlg1 = rangeAlg1
      ?yieldAlg2 = yieldAlg2
      ?rangeAlg2 = rangeAlg2
  in let
  
  (nil,copy,copy') = algebra
     
  s = tabulated1 $
      copy <<< c >>>| id 
  
  rewriteCopy [a',a'',c1,c2] = ([a',c1],[a'',c2])
  c = tabulated2 $
      copy' <<< 'a' ~~~ 'a' ~~~|| c >>>|| rewriteCopy |||
      copy' <<< 'b' ~~~ 'b' ~~~|| c >>>|| rewriteCopy |||
      nil   <<< (EPS,EPS) >>>|| id2
      
  z = mk inp
  tabulated1 = table1 z
  tabulated2 = table2 z
  
  in axiom z s