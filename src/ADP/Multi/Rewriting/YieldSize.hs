module ADP.Multi.Rewriting.YieldSize where

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import ADP.Debug
import ADP.Multi.Parser


doDetermineYieldSize ::  ([(Int, Int)] -> ([(Int, Int)], [(Int, Int)])) -> [ParserInfo2] -> ParserInfo2
doDetermineYieldSize f infos =
        let parserCount = length infos
            args = concatMap (\ x -> [(x,1),(x,2)]) [1..parserCount] 
            (left,right) = f args
            elemInfo = buildInfoMap infos
            leftYields = map (\(i,j) -> elemInfo Map.! (i,j)) left
            rightYields = map (\(i,j) -> elemInfo Map.! (i,j)) right
            (leftMin,leftMax) = combineYields leftYields
            (rightMin,rightMax) = combineYields rightYields 
        in trace (show elemInfo) $
           trace (show left) $
           trace (show right) $
           ParserInfo2 { 
                minYield = (leftMin,rightMin),
                maxYield = (leftMax,rightMax)
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

-- the input list is in reverse order, i.e. the first in the list is the last applied parser
buildInfoMap :: [ParserInfo2] -> InfoMap
buildInfoMap i | trace ("buildInfoMap " ++ show i) False = undefined
buildInfoMap infos =
        let parserCount = length infos
            list = concatMap (\ (x,info) -> 
                       [ ((x,1), (fst $ minYield info, fst $ maxYield info) ) 
                       , ((x,2), (snd $ minYield info, snd $ maxYield info) ) 
                       ]
                     ) $ zip [parserCount,parserCount-1..] infos
        in Map.fromList list