import Criterion.Main
import Criterion.Helpers

import ADP.Tests.Nussinov as Nuss
import ADP.Tests.NussinovExample as Nuss2

import BioInf.GAPlike as Nuss3
     
-- TODO try to adapt ADPfusion test so that the grammar/algebra is the same

-- run with -o report.html -u report.csv  
main :: IO ()
main = defaultMain
          [
              bgroup "nussinov78 (Haskell-ADP)" (benchArray (Nuss.nussinov78' Nuss.pairmax) inputs),
              bgroup "nussinov78 (adp-multi)" (benchArray (Nuss2.nussinov78 Nuss2.pairmax) inputs),
              bgroup "nussinov78 (ADPfusion)" (benchArray (fst . Nuss3.nussinov78) inputs)
          ]
     where
        longInp = "ggcguaggcgccgugcuuuugcuccccgcgcgcuguuuuucucgcugacuuucagcgggcggaaaagccucggccugccgccuuccaccguucauucuag"
        infiniteInp = cycle longInp
        
        inputs = [ (show i, take i infiniteInp) | i <- [100,200..1000] ]