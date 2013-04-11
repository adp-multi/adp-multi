{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module ADP.Multi.Rewriting.Tests.YieldSize (
    prop_yieldSizeMapSize,
    prop_yieldSizeMapElements,
    prop_yieldSizeDim2
) where

import Test.QuickCheck
import Text.Show.Functions()
import System.Random.Shuffle

import qualified Data.Map as Map

import ADP.Multi.Parser
import ADP.Multi.Rewriting.Model
import ADP.Multi.Rewriting.YieldSize

elemCount ParserInfo1{} = 1
elemCount ParserInfo2{} = 2
yieldSizeMapSize = foldl (\ count info -> count + elemCount info ) 0

prop_yieldSizeMapSize (infos :: [ParserInfo]) = 
    let yieldSizeMap = buildYieldSizeMap infos
    in Map.size yieldSizeMap == yieldSizeMapSize infos
    
prop_yieldSizeMapElements (infos :: [ParserInfo]) =
    let yieldSizeMap = buildYieldSizeMap infos
        reversed = reverse infos
        withIdx = zip [1..] reversed
        exists (i,ParserInfo1 {minYield=min1,maxYield=max1}) = 
            Map.member (i,1) yieldSizeMap && yieldSizeMap Map.! (i,1) == (min1,max1)
        exists (i,ParserInfo2 {minYield2=(min1,min2),maxYield2=(max1,max2)}) = 
            Map.member (i,1) yieldSizeMap && Map.member (i,2) yieldSizeMap &&
            yieldSizeMap Map.! (i,1) == (min1,max1) &&
            yieldSizeMap Map.! (i,2) == (min2,max2)
    in all exists withIdx
    
-- calculates the value of doDetermineYieldSize2 in terms of doDetermineYieldSize1
prop_yieldSizeDim2 (infos :: [ParserInfo]) =
    forAll (genDim2RewritingFunction infos) $ \ f ->
    let yieldSizeMap = buildYieldSizeMap infos
        (left,right) = f (Map.keys yieldSizeMap)
        yieldToInfo (minY,maxY) = ParserInfo1 {minYield = minY, maxYield = maxY}
        parserInfos = map (\(i,j) -> yieldToInfo $ yieldSizeMap Map.! (i,j))
        leftInfos = parserInfos left
        rightInfos = parserInfos right
        leftYield = determineYieldSize1 undefined leftInfos
        rightYield = determineYieldSize1 undefined rightInfos
    in determineYieldSize2 f infos 
       ==
       ParserInfo2 {
          minYield2 = (minYield leftYield, minYield rightYield),
          maxYield2 = (maxYield leftYield, maxYield rightYield)
       }

-- remove this once random-shuffle handles this case by itself
-- at the moment it goes into a <<loop>>!
_shuffle [] _ = []
_shuffle list samples = shuffle list samples

genDim2RewritingFunction :: [ParserInfo] -> Gen Dim2
genDim2RewritingFunction infos =
    let len = yieldSizeMapSize infos
    in do split <- choose (0,len)
          samples <- mapM (\i -> choose (0,len-i)) [1..len-1]
          return $ \ l ->
            let shuffled = _shuffle l samples
            in (take split shuffled, drop split shuffled)

    
instance Arbitrary ParserInfo where
    arbitrary = oneof [ do (minY,maxY) <- genMinMaxYield
                           return ParserInfo1 { minYield = minY, maxYield = maxY }
                        ,    
                        do (minY1,maxY1) <- genMinMaxYield
                           (minY2,maxY2) <- genMinMaxYield
                           return ParserInfo2 { minYield2 = (minY1,minY2), maxYield2 = (maxY1,maxY2) }
                      ]

genMinMaxYield :: Gen (Int,Maybe Int)
genMinMaxYield = sized $ \n -> 
                  do NonNegative minY <- arbitrary
                     maxY <- choose (minY,n)
                     oneof [ return (minY,Just maxY),
                             return (minY,Nothing) ]
                      