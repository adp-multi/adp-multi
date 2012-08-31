module ADP.Multi.Rewriting.YieldSize where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import ADP.Debug
import ADP.Multi.Parser
import ADP.Multi.Rewriting

{-
This module might later be re-integrated into both Rewriting implementations.
It is unclear yet if generically determining the yield size for higher parser
dimensions also needs a constraint solver.  
-}


doDetermineYieldSize1 ::  YieldAnalysisAlgorithm Dim1
doDetermineYieldSize1 f infos =
        let selfRecursion = ParserInfo1 { minYield = 0, maxYield = Nothing }
            elemInfo = buildInfoMap selfRecursion infos
            rewritten = f (Map.keys elemInfo)
            yields = map (\(i,j) -> elemInfo Map.! (i,j)) rewritten
            (yieldMin,yieldMax) = combineYields yields 
        in trace (show elemInfo) $
           trace (show yields) $
           ParserInfo1 { 
                minYield = yieldMin,
                maxYield = yieldMax
           }

doDetermineYieldSize2 ::  YieldAnalysisAlgorithm Dim2
doDetermineYieldSize2 f infos =
        let selfRecursion = ParserInfo2 { minYield2 = (0,0), maxYield2 = (Nothing,Nothing) }
            elemInfo = buildInfoMap selfRecursion infos
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
                    , if isNothing maxY1 || isNothing maxY2 
                      then Nothing
                      else Just $ fromJust maxY1 + fromJust maxY2
                    ) ) (0,Just 0)

type YieldSizes = (Int,Maybe Int) -- min and max yield sizes
type Info = YieldSizes -- could later be extended with more static analysis data
type InfoMap = Map (Int,Int) Info

-- | Replaces all instances of a value in a list by another value.
replace :: Eq a =>
           a   -- ^ Value to look for
        -> a   -- ^ Value to replace it with
        -> [a] -- ^ Input list
        -> [a] -- ^ Output list
replace x y = map (\z -> if z == x then y else z) 

-- the input list is in reverse order, i.e. the first in the list is the last applied parser
buildInfoMap :: ParserInfo -> [ParserInfo] -> InfoMap
buildInfoMap _ i | trace ("buildInfoMap " ++ show i) False = undefined
buildInfoMap recursionDefault infos' =
        let infos = replace ParserInfoSelf recursionDefault infos'
            parserCount = length infos
            list = concatMap (\ (x,info) -> case info of
                       ParserInfo1 { minYield = minY, maxYield = maxY } ->
                           [ ((x,1), (minY, maxY) ) ]
                       ParserInfo2 { minYield2 = minY, maxYield2 = maxY } ->
                           [ ((x,1), (fst minY, fst maxY) )
                           , ((x,2), (snd minY, snd maxY) )
                           ]
                     ) $ zip [parserCount,parserCount-1..] infos
        in Map.fromList list