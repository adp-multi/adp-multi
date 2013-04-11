-- | Calculates yield sizes using rewriting functions. 
module ADP.Multi.Rewriting.YieldSize where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (liftM2)

import ADP.Debug
import ADP.Multi.Parser
import ADP.Multi.Rewriting.Model

type YieldAnalysisAlgorithm a = a -> [ParserInfo] -> ParserInfo

-- for dim1 we don't need the rewriting function to determine the yield size
-- it's kept as argument anyway to make it more consistent
determineYieldSize1 ::  YieldAnalysisAlgorithm Dim1
determineYieldSize1 _ infos | trace ("determineYieldSize1 " ++ show infos) False = undefined
determineYieldSize1 _ infos =
        let elemInfo = buildInfoMap infos
            (yieldMin,yieldMax) = combineYields (Map.elems elemInfo) 
        in trace (show elemInfo) $
           ParserInfo1 { 
                minYield = yieldMin,
                maxYield = yieldMax
           }

determineYieldSize2 ::  YieldAnalysisAlgorithm Dim2
determineYieldSize2 _ infos | trace ("determineYieldSize2 " ++ show infos) False = undefined
determineYieldSize2 f infos =
        let elemInfo = buildInfoMap infos
            (left,right) = f (Map.keys elemInfo)
            leftYields = map (\(i,j) -> elemInfo Map.! (i,j)) left
            rightYields = map (\(i,j) -> elemInfo Map.! (i,j)) right
            (leftMin,leftMax) = combineYields leftYields
            (rightMin,rightMax) = combineYields rightYields 
        in trace (show elemInfo) $
           trace (show left) $
           trace (show right) $
           ParserInfo2 { 
                minYield2 = (leftMin,rightMin),
                maxYield2 = (leftMax,rightMax)
           }

combineYields :: [Info] -> Info
combineYields = foldl (\(minY1,maxY1) (minY2,maxY2) ->
                    ( minY1+minY2
                    , liftM2 (+) maxY1 maxY2
                    ) ) (0,Just 0)

type YieldSizes = (Int,Maybe Int) -- min and max yield sizes
type Info = YieldSizes -- could later be extended with more static analysis data
type InfoMap = Map (Int,Int) Info

-- the input list is in reverse order, i.e. the first in the list is the last applied parser
buildInfoMap :: [ParserInfo] -> InfoMap
buildInfoMap i | trace ("buildInfoMap " ++ show i) False = undefined
buildInfoMap infos =
        let parserCount = length infos
            list = concatMap (\ (x,info) -> case info of
                       ParserInfo1 { minYield = minY, maxYield = maxY } ->
                           [ ((x,1), (minY, maxY) ) ]
                       ParserInfo2 { minYield2 = minY, maxYield2 = maxY } ->
                           [ ((x,1), (fst minY, fst maxY) )
                           , ((x,2), (snd minY, snd maxY) )
                           ]
                     ) $ zip [parserCount,parserCount-1..] infos
        in Map.fromList list