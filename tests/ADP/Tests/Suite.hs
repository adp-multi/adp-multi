{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Test.Framework 
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.Monoid (mempty)

import Test.HUnit
import Test.QuickCheck

import Data.Char (toLower)
import Data.List

import ADP.Multi.Rewriting.Explicit
--import ADP.Multi.Rewriting.ConstraintSolver
import qualified ADP.Tests.RGExample as RG
import qualified ADP.Tests.RGExampleDim2 as RGDim2
import qualified ADP.Tests.CopyExample as Copy
import qualified ADP.Tests.CopyTwoTrackExample as CopyTT
import qualified ADP.Tests.NestedExample as Nested
import qualified ADP.Tests.OneStructureExample as One
import qualified ADP.Tests.ZeroStructureTwoBackbonesExample as ZeroTT

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
                        testCase "tests associative function with max basepairs" testRgSimpleBasepairs,
                        testProperty "produces copy language" prop_copyLanguage,
                        testProperty "produces copy language (two track)" prop_copyLanguageTT,
                        testProperty "produces nested rna" prop_nestedRna,
                        testProperty "produces 1-structure rna" prop_oneStructureRna,
                        testProperty "produces RG rna" prop_rgRna,
                        testProperty "produces RG (dim2) rna" prop_rgDim2Rna
--                        testProperty "produces 0-structure over two backbones rna" prop_zeroStructureTwoBackbonesRna
                    ]
            ]
       mempty {
            ropt_test_options = Just mempty {
                topt_maximum_generated_tests = Just 100
            }
       }
                
rg :: RG.RG_Algebra Char answer -> String -> [answer]
rg = RG.rgknot determineYieldSize1 constructRanges1 determineYieldSize2 constructRanges2

rgDim2 :: RGDim2.RG_Algebra Char answer -> String -> [answer]
rgDim2 = RGDim2.rgknot determineYieldSize1 constructRanges1 determineYieldSize2 constructRanges2

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

smallTestSize prop = sized $ \n -> resize (round (sqrt (fromIntegral n))) prop

prop_copyLanguage (CopyLangString w) =
    let result = Copy.copyGr determineYieldSize1 constructRanges1 determineYieldSize2 constructRanges2
                             Copy.prettyprint (w ++ w)
    in result == [[w ++ w]]

prop_copyLanguageTT (CopyLangString w) =
    let result = CopyTT.copyTTGr determineYieldSize2 constructRanges2 CopyTT.prettyprint (w,w)
    in result == [(w,w)]
    
prop_nestedRna (RNAString w) =
    let results = Nested.nested determineYieldSize1 constructRanges1 Nested.prettyprint w
    in not (null results) && all (\(_,result) -> result == w) results
    
prop_oneStructureRna (RNAString w) =
    let results = One.oneStructure determineYieldSize1 constructRanges1 determineYieldSize2 constructRanges2
                                   One.prettyprint2 w
    in not (null results) && all (\[result] -> result == w) results
    
prop_rgRna (RNAString w) =
    let results = rg RG.prettyprint w
    in not (null results) && all (\(_,[result]) -> result == w) results
    
prop_rgDim2Rna (RNAString w) =
    let results = rgDim2 RGDim2.prettyprint w
        resultsDim1 = rg RG.prettyprint w
    in results == resultsDim1
    
prop_zeroStructureTwoBackbonesRna (RNAString w) =
    let results = ZeroTT.zeroStructureTwoBackbones 
                        determineYieldSize1 constructRanges1 determineYieldSize2 constructRanges2
                        ZeroTT.enum (w,w)
    in not (null results)

                 
newtype CopyLangString = CopyLangString String deriving (Show)
instance Arbitrary CopyLangString where
    arbitrary = genAlphabetString CopyLangString "ab"
                   
newtype RNAString = RNAString String deriving (Show)
instance Arbitrary RNAString where
    arbitrary = genAlphabetString RNAString "agcu"

-- returns a small test string consisting of letters from an alphabet
genAlphabetString typ alph =
    sized $ \n ->
    do s <- mapM (\_ -> elements alph) [0..round (sqrt (fromIntegral n))]
       return $ typ s
