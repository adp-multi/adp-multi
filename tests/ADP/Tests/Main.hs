import System.IO (hSetBuffering, stdout, BufferMode (LineBuffering))
import Data.Char (toLower)
import Control.Monad (forM_)
import qualified ADP.Tests.RGExample as RG
import qualified ADP.Tests.NestedExample as N
import qualified ADP.Tests.CopyExample as C
import qualified ADP.Tests.OneStructureExample as One
import qualified ADP.Tests.ZeroStructureTwoBackbonesExample as ZeroTT
import qualified ADP.Tests.AlignmentExample as Alignment
import qualified ADP.Tests.TermExample as Term
--import ADP.Multi.Rewriting.ConstraintSolver
import ADP.Multi.Rewriting.Explicit


main::IO()
main = do
        hSetBuffering stdout LineBuffering
        
        --forM_ result print
        --forM_ result21 print
        --forM_ result3 print
        --forM_ result4 print
        --forM_ result53 print
        --forM_ result6 putStrLn
        --forM_ result6t3 print
        --forM_ result7 print
        --forM_ result8 print
        --forM_ result9 print
        --forM_ result10 print
        forM_ resultTerm putStrLn
        
        where
            -- http://www.ekevanbatenburg.nl/PKBASE/PKB00279.HTML
            -- struc = ".(((((..[[[))))).]]]."
            --inp = map toLower "CAAUUUUCUGAAAAUUUUCAC"
            
            -- http://www.ekevanbatenburg.nl/PKBASE/PKB00289.HTML
            -- struc = "..((((..[[[[)))).....]]]]..."
            -- inp = map toLower "ACCGUCGUUCCCGACGUAAAAGGGAUGU"
            
            -- https://github.com/neothemachine/rna/wiki/Example
            inp = "agcgu"

            --inp = map toLower "ACGAUUCAACGU"
            
            rg = RG.rgknot determineYieldSize1 constructRanges1 determineYieldSize2 constructRanges2
            
            result = rg RG.enum inp
            result2 = rg RG.maxBasepairs inp
            result21 = rg (RG.prettyprint RG.*** RG.maxBasepairs) inp
            result3 = rg RG.maxKnots inp
            result4 = rg RG.prettyprint inp
            
            result5 = rg (RG.enum RG.*** RG.prettyprint) inp
            result51 = rg (RG.prettyprint RG.*** RG.pstree) inp
            result52 = rg (RG.prettyprint RG.*** RG.pstreeYield) inp
            result53 = rg (RG.prettyprint RG.*** RG.pstreeEval) inp
            
            nested = N.nested determineYieldSize1 constructRanges1
            result6 = nested (N.pstree) inp
            
            result6t = nested (N.maxBasepairs N.*** N.prettyprint) inp
            result6t2 = nested (N.term N.*** N.maxBasepairs) inp
            result6t3 = nested (N.termPlain N.*** N.maxBasepairs) inp
            
            copy = C.copyGr determineYieldSize1 constructRanges1 determineYieldSize2 constructRanges2
            result7 = copy (C.countABs) "abaaabaa"
            
            oneStructure = One.oneStructure determineYieldSize1 constructRanges1 determineYieldSize2 constructRanges2
            result8 = oneStructure (One.prettyprint) inp
            
            zeroStructureTT = ZeroTT.zeroStructureTwoBackbones determineYieldSize1 constructRanges1 determineYieldSize2 constructRanges2
            result9 = zeroStructureTT (ZeroTT.enum) (inp,inp)
            
            alignment = Alignment.alignmentGr determineYieldSize2 constructRanges2
            result10 = alignment (Alignment.unit Alignment.*** Alignment.count) ("darling","airline")
            
            term = Term.term determineYieldSize1 constructRanges1
            termAlg = Term.qtree (\sym -> 
                if sym `elem` ["a","g","c","u",".","(",")","[","]"] then "\\ts{" ++ sym ++ "}" 
                else if take 2 sym == "f_" || take 2 sym == "g_" then "$\\op{" ++ take 1 sym ++ "}_" ++ drop 2 sym ++ "$"
                else "$" ++ sym ++ "$")
            resultTerm = term termAlg "Z:r_1(.,Z:r_1(.,Z:r_1(.,Z:r_1(.,Z:r_1(.,Z:r_1(.,\\epsilon))))))"