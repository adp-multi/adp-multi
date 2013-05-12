import Criterion.Main
import Criterion.Helpers

import ADP.Tests.RGExample as RG
     
-- run with -u report.csv
main :: IO ()
main = defaultMain
          [
              bgroup "C2u (adp-multi)" (benchArray (RG.rgknot RG.maxBasepairs) inputs)
          ]
     where
        longInp = "ggcguaggcgccgugcuuuugcuccccgcgcgcuguuuuucucgcugacuuucagcgggcggaaaagccucggccugccgccuuccaccguucauucuag"
        infiniteInp = cycle longInp
        inputs = [ (show i, take i infiniteInp) | i <- [10,15..50] ]