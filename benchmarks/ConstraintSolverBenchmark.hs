import Criterion.Main

import ADP.Multi.Rewriting.ConstraintSolver as CS
import ADP.Multi.Rewriting.Explicit as EX
import ADP.Tests.RGExample as RG
import ADP.Tests.RGExampleDim2 as RG2

main :: IO ()
main = defaultMain 
          [
              bgroup "RG dim1+2" [
                     bench "explicit" $ nf (simple EX.determineYieldSize1 EX.constructRanges1 EX.determineYieldSize2) EX.constructRanges2,
                     bench "constraint solver" $ nf (simple CS.determineYieldSize1 CS.constructRanges1 CS.determineYieldSize2) CS.constructRanges2
                ],
              bgroup "RG dim2" [
                     bench "explicit" $ nf (simple EX.determineYieldSize1 EX.constructRanges1 EX.determineYieldSize2) EX.constructRanges2,
                     bench "constraint solver" $ nf (simple CS.determineYieldSize1 CS.constructRanges1 CS.determineYieldSize2) CS.constructRanges2
                ]
          ]
       

simple yieldAlg1 rangeAlg1 yieldAlg2 rangeAlg2 = 
       RG.rgknot yieldAlg1 rangeAlg1 yieldAlg2 rangeAlg2 RG.maxBasepairs "agcgu"
       
simple2 yieldAlg1 rangeAlg1 yieldAlg2 rangeAlg2 = 
       RG2.rgknot yieldAlg1 rangeAlg1 yieldAlg2 rangeAlg2 RG2.maxBasepairs "agcgu"