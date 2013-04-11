{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Test.Framework 
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.Monoid (mempty)

import Test.HUnit
import Test.QuickCheck

import Data.Char (toLower)

import qualified ADP.Tests.RGExample as RG
import qualified ADP.Tests.RGExampleDim2 as RGDim2
import qualified ADP.Tests.RGExampleStar as RGStar
import qualified ADP.Tests.CopyExample as Copy
import qualified ADP.Tests.CopyTwoTrackExample as CopyTT
import qualified MCFG.MCFG as MCFG
import qualified ADP.Tests.NestedExample as Nested
import qualified ADP.Tests.OneStructureExample as One
import qualified ADP.Tests.ZeroStructureTwoBackbonesExample as ZeroTT

import ADP.Multi.Rewriting.Tests.YieldSize

main :: IO ()
main = defaultMainWithOpts
            [
                testGroup "Property tests" [
                    testGroup "Yield size" [
                        testProperty "map size" prop_yieldSizeMapSize,
                        testProperty "map elements" prop_yieldSizeMapElements,
                        testProperty "yield size" prop_yieldSizeDim2
                        ]
                    ],
                testGroup "System tests" [
                        testCase "finds all reference structures" testRgSimpleCompleteness,
                      --testCase "finds pseudoknot reference structure" testRgRealPseudoknot,
                        testCase "tests associative function with max basepairs" testRgSimpleBasepairs,
                        testProperty "produces copy language" prop_copyLanguage,
                        testProperty "produces same derivation trees for copy language grammar" prop_copyLanguageDerivation,
                        testProperty "produces copy language (two track)" prop_copyLanguageTT,
                        testProperty "produces nested rna" prop_nestedRna,
                        testProperty "produces 1-structure rna" prop_oneStructureRna,
                        testProperty "produces RG rna" prop_rgRna,
                        testProperty "produces RG (dim2) rna" prop_rgDim2Rna,
                        testProperty "produces RG (star version) rna" prop_rgStarRna,
                        testProperty "produces 0-structure over two backbones rna" prop_zeroStructureTwoBackbonesRna
                    ]
            ]
       mempty {
            ropt_test_options = Just mempty {
                topt_maximum_generated_tests = Just 100
            }
       }
                
rg :: RG.RG_Algebra Char answer -> String -> [answer]
rg = RG.rgknot

rgDim2 :: RGDim2.RG_Algebra Char answer -> String -> [answer]
rgDim2 = RGDim2.rgknot

rgStar :: RGStar.RG_Algebra Char answer -> String -> [answer]
rgStar = RGStar.rgknot

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
    let result = Copy.copyGr Copy.prettyprint (w ++ w)
    in result == [w ++ w]

prop_copyLanguageTT (CopyLangString w) =
    let result = CopyTT.copyTTGr CopyTT.prettyprint (w,w)
    in result == [(w,w)]

-- this basically checks if the yield parser of adp-multi produces the same derivation trees
-- as the MCFG parser by Johannes Waldmann
-- Note: the copy language grammar is unambiguous! thus, ambiguous grammars (=multiple trees) are not tested here
prop_copyLanguageDerivation (CopyLangString w) =
    let [resultADP] = Copy.copyGr Copy.derivation (w ++ w)
        [resultMCFG] = MCFG.parse Copy.mcfg (map MCFG.T (w ++ w))
    in MCFG.consistent resultMCFG && equivalentTrees resultADP resultMCFG

-- checks if two derivation trees are the same (same rules applied)
equivalentTrees :: MCFG.Derivation -> MCFG.Derivation -> Bool
equivalentTrees t1 t2 =
    let MCFG.Derivation _ rule1 children1 = t1
        MCFG.Derivation _ rule2 children2 = t2
        children = zip children1 children2
    in rule1 == rule2 && 
       length children1 == length children2 &&
       all (\(c1,c2) -> equivalentTrees c1 c2) children
    
prop_nestedRna (RNAString w) =
    let results = Nested.nested Nested.prettyprint w
    in not (null results) && all (\(_,result) -> result == w) results
    
prop_oneStructureRna (RNAString w) =
    let results = One.oneStructure One.prettyprint2 w
    in not (null results) && all (\[result] -> result == w) results
    
prop_rgRna (RNAString w) =
    let results = rg RG.prettyprint w
    in not (null results) && all (\(_,[result]) -> result == w) results
    
prop_rgDim2Rna (RNAString w) =
    let results = rgDim2 RGDim2.prettyprint w
        resultsDim1 = rg RG.prettyprint w
    in results == resultsDim1
    
prop_rgStarRna (RNAString w) =
    let results = rgStar RGStar.prettyprint w
        resultsRef = rg RG.prettyprint w
    in results == resultsRef

-- This test is a bit useless, it just shows that "something" happens.
-- TODO: as in the other tests, we would need a pretty-printing algebra 
prop_zeroStructureTwoBackbonesRna (RNAString w) =
    let results = ZeroTT.zeroStructureTwoBackbones ZeroTT.enum (w,w)
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
