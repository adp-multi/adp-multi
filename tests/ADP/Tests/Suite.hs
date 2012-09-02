{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.HUnit
import Test.QuickCheck

import Data.Char (toLower)
import Data.List

import ADP.Multi.Rewriting.ConstraintSolver
import qualified ADP.Tests.RGExample as RG

main :: IO ()
main = defaultMain
            [
                testGroup "Unit tests" [
                        testProperty "sort1" prop_sort1,
                        testProperty "sort2" prop_sort4
                    ],
                testGroup "System tests" [
                        testCase "finds all reference structures" testRgSimpleCompleteness,
                      --testCase "finds pseudoknot reference structure" testRgRealPseudoknot,
                        testCase "tests associative function with max basepairs" testRgSimpleBasepairs
                    ]
            ]

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
   let inp = map toLower "CAAUUUUCUGAAAAUUUUCAC" 
       referenceStructure = ".(((((.[[[))))).]]]."
       result = rg RG.prettyprint inp
   in any (\ ([structure],_) -> structure == referenceStructure) result
        @? "reference structure not found"

prop_sort1 (xs :: [Int]) = sort xs == sortBy compare xs

prop_sort4 (xs :: [Int]) (ys :: [Int]) =
        (not (null xs)) ==>
        (not (null ys)) ==>
        (head (sort (xs ++ ys)) == min (minimum xs) (minimum ys))
