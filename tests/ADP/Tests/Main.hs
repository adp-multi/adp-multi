import System.IO (hSetBuffering, stdout, BufferMode (LineBuffering))
import Data.Char (toLower)
import Control.Monad (forM_)
import qualified ADP.Tests.RGExample as RG
--import ADP.Multi.Rewriting.ConstraintSolver
import ADP.Multi.Rewriting.Explicit


main::IO()
main = do
        hSetBuffering stdout LineBuffering
        
        --forM_ result print
        forM_ result2 print
        --forM_ result3 print
        --forM_ result4 print
        --forM_ result5 print
        
        where
            -- http://www.ekevanbatenburg.nl/PKBASE/PKB00279.HTML
            -- struc = ".(((((.[[[))))).]]]."
            -- inp = map toLower "CAAUUUUCUGAAAAUUUUCAC"
            
            -- http://www.ekevanbatenburg.nl/PKBASE/PKB00289.HTML
            -- longer, but produces results earlier
            -- struc = "..((((..[[[[)))).....]]]]..."
            -- inp = map toLower "ACCGUCGUUCCCGACGUAAAAGGGAUGU"
            
            -- https://github.com/neothemachine/rna/wiki/Example
            inp = "agcgu"
            
            --inp = map toLower "ACGAUUCAACGU"
            
            rg = RG.rgknot determineYieldSize constructRanges
            
            result = rg RG.enum inp
            result2 = rg RG.maxBasepairs inp
            result3 = rg RG.maxKnots inp
            result4 = rg RG.prettyprint inp
            
            result5 = rg (RG.enum RG.*** RG.prettyprint) inp