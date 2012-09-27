import System.IO (hSetBuffering, stdout, BufferMode (LineBuffering))
import Data.Char (toLower)
import Control.Monad (forM_)
import qualified ADP.Tests.RGExample as RG
import qualified ADP.Tests.NestedExample as N
import qualified ADP.Tests.CopyExample as C
import qualified ADP.Tests.OneStructureExample as One
import qualified ADP.Tests.ZeroStructureTwoBackbonesExample as ZeroTT
--import ADP.Multi.Rewriting.ConstraintSolver
import ADP.Multi.Rewriting.Explicit


main::IO()
main = do
        hSetBuffering stdout LineBuffering
        
        --forM_ result print
        --forM_ result2 print
        --forM_ result3 print
        --forM_ result4 print
        --forM_ result5 print
        --forM_ result6 putStrLn
        --forM_ result7 print
        --forM_ result8 print
        forM_ result9 print
        
        where
            -- http://www.ekevanbatenburg.nl/PKBASE/PKB00279.HTML
            -- struc = ".(((((..[[[))))).]]]."
            --inp = map toLower "CAAUUUUCUGAAAAUUUUCAC"
            
            -- http://www.ekevanbatenburg.nl/PKBASE/PKB00289.HTML
            -- struc = "..((((..[[[[)))).....]]]]..."
            -- inp = map toLower "ACCGUCGUUCCCGACGUAAAAGGGAUGU"
            
            -- https://github.com/neothemachine/rna/wiki/Example
            -- inp = "agcgu"
            
            inp = map toLower "ACGAUUCAACGU"
            
            rg = RG.rgknot determineYieldSize1 constructRanges1 determineYieldSize2 constructRanges2
            
            result = rg RG.enum inp
            result2 = rg RG.maxBasepairs inp
            result3 = rg RG.maxKnots inp
            result4 = rg RG.prettyprint inp
            
            result5 = rg (RG.enum RG.*** RG.prettyprint) inp
            
            nested = N.nested determineYieldSize1 constructRanges1
            result6 = nested (N.pstree) inp
            
            copy = C.copyGr determineYieldSize1 constructRanges1 determineYieldSize2 constructRanges2
            result7 = copy (C.countABs) "abaaabaa"
            
            oneStructure = One.oneStructure determineYieldSize1 constructRanges1 determineYieldSize2 constructRanges2
            result8 = oneStructure (One.prettyprint) inp
            
            zeroStructureTT = ZeroTT.zeroStructureTwoBackbones determineYieldSize1 constructRanges1 determineYieldSize2 constructRanges2
            result9 = zeroStructureTT (ZeroTT.enum) (inp,inp)