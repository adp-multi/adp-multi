import Criterion.Main

import ADP.Multi.Rewriting.ConstraintSolver as CS
import ADP.Multi.Rewriting.Explicit as EX
import ADP.Tests.RGExample as RG
import ADP.Tests.RGExampleDim2 as RG2
import ADP.Tests.Nussinov as Nuss
import ADP.Tests.NussinovExample as Nuss2

main :: IO ()
main = defaultMain 
          [
              bgroup "compare explicit vs constraint solver range construction" [
                  bgroup "RG dim1+2" [
                         bench "explicit" $ nf (simple EX.determineYieldSize1 EX.constructRanges1 EX.determineYieldSize2) EX.constructRanges2,
                         bench "constraint solver" $ nf (simple CS.determineYieldSize1 CS.constructRanges1 CS.determineYieldSize2) CS.constructRanges2
                    ],
                  bgroup "RG dim2" [
                         bench "explicit" $ nf (simple2 EX.determineYieldSize1 EX.constructRanges1 EX.determineYieldSize2) EX.constructRanges2,
                         bench "constraint solver" $ nf (simple2 CS.determineYieldSize1 CS.constructRanges1 CS.determineYieldSize2) CS.constructRanges2
                    ]
              ],
              bgroup "compare different nussinovs" [
                  bench "nussinov78 (adp)" $ nf (Nuss.nussinov78 Nuss.pairmax) longInp,
                  bench "nussinov78' (adp)" $ nf (Nuss.nussinov78' Nuss.pairmax) longInp,
                  bench "nussinov78 (adp-multi)" $ nf (Nuss2.nussinov78 EX.determineYieldSize1 EX.constructRanges1 Nuss2.pairmax) longInp
              ],
              bgroup "compare different nussinovs (doubled size)" [
                  bench "nussinov78 (adp)" $ nf (Nuss.nussinov78 Nuss.pairmax) veryLongInp,
                  bench "nussinov78' (adp)" $ nf (Nuss.nussinov78' Nuss.pairmax) veryLongInp,
                  bench "nussinov78 (adp-multi)" $ nf (Nuss2.nussinov78 EX.determineYieldSize1 EX.constructRanges1 Nuss2.pairmax) veryLongInp
              ]
          ]

longInp = "ggcguaggcgccgugcuuuugcuccccgcgcgcuguuuuucucgcugacuuucagcgggcggaaaagccucggccugccgccuuccaccguucauucuagagcaaacaaaaaaugucagcu"
veryLongInp = longInp ++ longInp       

simple yieldAlg1 rangeAlg1 yieldAlg2 rangeAlg2 = 
       RG.rgknot yieldAlg1 rangeAlg1 yieldAlg2 rangeAlg2 RG.maxBasepairs "agcgu"
       
simple2 yieldAlg1 rangeAlg1 yieldAlg2 rangeAlg2 = 
       RG2.rgknot yieldAlg1 rangeAlg1 yieldAlg2 rangeAlg2 RG2.maxBasepairs "agcgu"