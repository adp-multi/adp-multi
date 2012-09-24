{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module ADP.Multi.Rewriting.Tests.YieldSize (
    prop_infoMapSize,
    prop_infoMapElements,
    prop_yieldSizeDim2
) where

import Test.QuickCheck
import Text.Show.Functions()
import System.Random.Shuffle

import qualified Data.Map as Map

import ADP.Multi.Parser
import ADP.Multi.Rewriting
import ADP.Multi.Rewriting.YieldSize

elemCount ParserInfo1{} = 1
elemCount ParserInfo2{} = 2
infoMapSize = foldl (\ count info -> count + elemCount info ) 0

prop_infoMapSize (infos :: [ParserInfo]) = 
    let infoMap = buildInfoMap infos
    in Map.size infoMap == infoMapSize infos
    
prop_infoMapElements (infos :: [ParserInfo]) =
    let infoMap = buildInfoMap infos
        reversed = reverse infos
        withIdx = zip [1..] reversed
        exists (i,ParserInfo1 {minYield=min1,maxYield=max1}) = 
            Map.member (i,1) infoMap && infoMap Map.! (i,1) == (min1,max1)
        exists (i,ParserInfo2 {minYield2=(min1,min2),maxYield2=(max1,max2)}) = 
            Map.member (i,1) infoMap && Map.member (i,2) infoMap &&
            infoMap Map.! (i,1) == (min1,max1) &&
            infoMap Map.! (i,2) == (min2,max2)
    in all exists withIdx
    
-- calculates the value of doDetermineYieldSize2 in terms of doDetermineYieldSize1
prop_yieldSizeDim2 (infos :: [ParserInfo]) =
    forAll (genDim2RewritingFunction infos) $ \ f ->
    let elemInfo = buildInfoMap infos
        (left,right) = f (Map.keys elemInfo)
        yieldToInfo (minY,maxY) = ParserInfo1 {minYield = minY, maxYield = maxY}
        parserInfos = map (\(i,j) -> yieldToInfo $ elemInfo Map.! (i,j))
        leftInfos = parserInfos left
        rightInfos = parserInfos right
        leftYield = doDetermineYieldSize1 undefined leftInfos
        rightYield = doDetermineYieldSize1 undefined rightInfos
    in doDetermineYieldSize2 f infos 
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
    let len = infoMapSize infos
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
                      