{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Test.Framework 
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.Monoid (mempty)

import Test.HUnit

import Data.Char (toLower)
import Data.List

import ADP.Multi.Rewriting.Explicit
import qualified ADP.Tests.RGExample as RG

import ADP.Multi.Rewriting.Tests.YieldSize

main :: IO ()
main = defaultMainWithOpts
            [
                testGroup "Property tests" [
                    testGroup "Yield size" [
                        testProperty "map size" prop_infoMapSize,
                        testProperty "map elements" prop_infoMapElements,
                        testProperty "yield size" prop_yieldSizeDim2
                        ]
                    ],
                testGroup "System tests" [
                        testCase "finds all reference structures" testRgSimpleCompleteness,
                      --testCase "finds pseudoknot reference structure" testRgRealPseudoknot,
                        testCase "tests associative function with max basepairs" testRgSimpleBasepairs
                    ]
            ]
       mempty {
            ropt_test_options = Just mempty {
                topt_maximum_generated_tests = Just 1000
            }
       }
                
rg :: RG.RG_Algebra Char answer -> String -> [answer]
rg = RG.rgknot determineYieldSize1 constructRanges1 determineYieldSize2 constructRanges2

-- https://github.com/neothemachine/rna/wiki/Example
testRgSimpleCompleteness =
   let inp = "agcgu"
       referenceStructures = [
                ".....",
                ".()..",
                "...()",
                "..().",
                ".()()",
                ".(..)",
                ".(())",
                "(...)",
                "(().)",
                "(.())"
          ]
       result = rg RG.prettyprint inp
   in do length result @?= length referenceStructures
         all (\ ([structure],_) -> structure `elem` referenceStructures) result
           @? "reference structure not found"
           
-- https://github.com/neothemachine/rna/wiki/Example
testRgSimpleBasepairs =
   let inp = "agcgu"
       [maxBasepairs] = rg RG.maxBasepairs inp
   in maxBasepairs @?= 2

-- http://www.ekevanbatenburg.nl/PKBASE/PKB00279.HTML
-- This test runs quite long and should only be run manually if needed.
testRgRealPseudoknot =
   let inp = map toLower     "CAAUUUUCUGAAAAUUUUCAC" 
       referenceStructure  = ".(((((..[[[))))).]]]."
       referenceStructure2 = ".[[[[[..(((]]]]].)))."
       result = rg RG.prettyprint inp
   in any (\ ([structure],_) -> structure == referenceStructure || structure == referenceStructure2) result
        @? "reference structure not found"

