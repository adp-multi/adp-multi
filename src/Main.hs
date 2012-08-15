module Main where
import qualified ADP.Nussinov as N 
import qualified ADP.Multi.RGExample as RG
import qualified ADP.Multi.Combinators as T

main::IO()
main = do
          print T.test 
{-        print $ show result where
                result = N.nussinov78 N.enum "aggca" ;
-}
--        print $ show result where
--                result = RG.rgknot RG.enum "aggca"