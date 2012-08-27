{-# LANGUAGE ScopedTypeVariables #-}

import Criterion.Main

import ADP.Multi.Rewriting.ConstraintSolver as CS
import ADP.Multi.Rewriting.Explicit as EX
import qualified ADP.Tests.RGExample as RG

main :: IO ()
main = defaultMain 
          [
              bgroup "quick" [
                     bench "explicit" $ nf (simple EX.determineYieldSize) EX.constructRanges,
                     bench "constraint solver" $ nf (simple CS.determineYieldSize) CS.constructRanges
                ]
          ]
       

simple yieldAlg rangeAlg = 
       RG.rgknot yieldAlg rangeAlg RG.maxBasepairs "agcgu"