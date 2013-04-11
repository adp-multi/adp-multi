module ADP.Tests.NussinovExample where

import ADP.Multi.All
import ADP.Multi.Rewriting.All
                                 
type Nussinov_Algebra alphabet answer = (
   EPS -> answer,                              -- nil
   alphabet -> answer,                         -- base
   alphabet -> answer   -> answer,             -- left
   answer   -> answer   -> answer,             -- right
   alphabet -> answer   -> alphabet -> answer, -- pair
   answer   -> answer   -> answer,             -- split
   [answer] -> [answer]                        -- h
   )
   
pairmax :: Nussinov_Algebra Char Int
pairmax = (nil,base,left,right,pair,split,h) where
    nil _       = 0
    base _      = undefined
    left _ x    = x
    right x _   = x
    pair _ x _  = x + 1
    split x y   = x + y
    h xs        = [maximum xs]
  
   
nussinov78 :: Nussinov_Algebra Char answer -> String -> [answer]
nussinov78 algebra inp =
  let  
  (nil,base,left,right,pair,split,h) = algebra

  s = tabulated $
      yieldSize1 (0, Nothing) $
      nil <<< EPS >>> id1 |||
      right <<< s ~~~ b >>> id1 |||
      split <<< s ~~~ t >>> id1
      ... h

  t = tabulated $
      pair <<< 'a' ~~~ s ~~~ 'u' >>> id1 |||
      pair <<< 'u' ~~~ s ~~~ 'a' >>> id1 |||
      pair <<< 'c' ~~~ s ~~~ 'g' >>> id1 |||
      pair <<< 'g' ~~~ s ~~~ 'c' >>> id1 |||
      pair <<< 'g' ~~~ s ~~~ 'u' >>> id1 |||
      pair <<< 'u' ~~~ s ~~~ 'g' >>> id1

  b = tabulated $
      base <<< 'a' >>> id1 |||
      base <<< 'u' >>> id1 |||
      base <<< 'c' >>> id1 |||
      base <<< 'g' >>> id1
  
  z = mk inp
  tabulated = table1 z
  
  in axiom z s