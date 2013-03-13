{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

{-
Use monadiccp as a finite-domain constraint solver to construct
subwords in a generic way.

TODO It is slow as hell. Maybe it is possible to "compile" the two inequality
     systems so that they can later be run faster.
     see http://www.cs.washington.edu/research/constraints/solvers/cp97.html
-}
module ADP.Multi.Rewriting.ConstraintSolver (
        determineYieldSize1,
        determineYieldSize2,
        constructRanges1,
        constructRanges2
) where

import Control.Exception
import Data.List (elemIndex, find)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isNothing)

import ADP.Debug
import ADP.Multi.Parser
import ADP.Multi.Rewriting
import ADP.Multi.Rewriting.YieldSize

import ADP.Multi.Rewriting.MonadicCpHelper
import Control.CP.FD.Interface

type Subword1 = (Int,Int)
type Subword2 = (Int,Int,Int,Int)

constructRanges1 :: RangeConstructionAlgorithm Dim1
constructRanges1 _ _ b | trace ("constructRanges1 " ++ show b) False = undefined
constructRanges1 f infos [i,j] =
        assert (i <= j) $
        let parserCount = length infos            
            elemInfo = buildInfoMap infos
            rewritten = f (Map.keys elemInfo)
            remainingSymbols = [parserCount,parserCount-1..1] `zip` infos
            rangeDesc = [(i,j,rewritten)]
            rangeDescFiltered = filterEmptyRanges rangeDesc
        in trace (show remainingSymbols) $
           if any (\(m,n,d) -> null d && m /= n) rangeDesc then []
           else constructRangesRec elemInfo remainingSymbols rangeDescFiltered

constructRanges2 :: RangeConstructionAlgorithm Dim2
constructRanges2 _ _ b | trace ("constructRanges2 " ++ show b) False = undefined
constructRanges2 f infos [i,j,k,l] =
        assert (i <= j && j <= k && k <= l) $
        let parserCount = length infos
            elemInfo = buildInfoMap infos
            (left,right) = f (Map.keys elemInfo)
            remainingSymbols = [parserCount,parserCount-1..1] `zip` infos
            rangeDesc = [(i,j,left),(k,l,right)]
            rangeDescFiltered = filterEmptyRanges rangeDesc
        in if any (\(m,n,d) -> null d && m /= n) rangeDesc then []
           else constructRangesRec elemInfo remainingSymbols rangeDescFiltered

determineYieldSize1 :: YieldAnalysisAlgorithm Dim1
determineYieldSize1 _ infos | trace ("determineYieldSize1 " ++ show infos) False = undefined
determineYieldSize1 f infos = doDetermineYieldSize1 f infos

determineYieldSize2 :: YieldAnalysisAlgorithm Dim2
determineYieldSize2 _ infos | trace ("determineYieldSize2 " ++ show infos) False = undefined
determineYieldSize2 f infos = doDetermineYieldSize2 f infos


type RangeDesc = (Int,Int,[(Int,Int)])

constructRangesRec :: InfoMap -> [(Int,ParserInfo)] -> [RangeDesc] -> [Ranges]
constructRangesRec a b c | trace ("constructRangesRec " ++ show a ++ " " ++ show b ++ " " ++ show c) False = undefined
constructRangesRec _ [] [] = []
constructRangesRec infoMap ((current,ParserInfo2 {}):rest) rangeDescs =
        let symbolLoc = findSymbol2 current rangeDescs
            subwords = calcSubwords2 infoMap symbolLoc
        in trace ("calc subwords for dim2") $
           trace ("subwords: " ++ show subwords) $
           [ RangeMap [i,j,k,l] restRanges |
             (i,j,k,l) <- subwords,
             let newDescs = constructNewRangeDescs2 rangeDescs symbolLoc (i,j,k,l),
             let restRanges = constructRangesRec infoMap rest newDescs
           ]
constructRangesRec infoMap ((current,ParserInfo1 {}):rest) rangeDescs =
        let symbolLoc = findSymbol1 current rangeDescs
            subwords = calcSubwords1 infoMap symbolLoc
        in trace ("calc subwords for dim1") $
           trace ("subwords: " ++ show subwords) $
           [ RangeMap [i,j] restRanges |
             (i,j) <- subwords,
             let newDescs = constructNewRangeDescs1 rangeDescs symbolLoc (i,j),
             let restRanges = constructRangesRec infoMap rest newDescs
           ]
constructRangesRec _ [] r@(_:_) = error ("programming error " ++ show r)

findSymbol :: Int -> Int -> [RangeDesc] -> (RangeDesc,Int)
findSymbol s idx r | trace ("findSymbol " ++ show s ++ "," ++ show idx ++ " " ++ show r) False = undefined
findSymbol s idx rangeDesc =
         let Just (i,j,r)  = find (\(_,_,l') -> any (\(s',i') -> s' == s && i' == idx) l') rangeDesc
             Just aIdx = elemIndex (s,idx) r
         in ((i,j,r),aIdx)

findSymbol1 :: Int -> [RangeDesc] -> (RangeDesc,Int)
findSymbol1 s = findSymbol s 1

findSymbol2 :: Int -> [RangeDesc] -> ((RangeDesc,Int),(RangeDesc,Int))
findSymbol2 s rangeDesc = (findSymbol s 1 rangeDesc, findSymbol s 2 rangeDesc)

-- TODO refactor (code duplication with Explicit module)

constructNewRangeDescs1 :: [RangeDesc] -> (RangeDesc,Int) -> Subword1 -> [RangeDesc]
constructNewRangeDescs1 d p s | trace ("constructNewRangeDescs " ++ show d ++ " " ++ show p ++ " " ++ show s) False = undefined
constructNewRangeDescs1 descs symbolPosition subword =
        let newDescs = [ newDesc |
                         desc <- descs
                       , newDesc <- processRangeDesc1 desc symbolPosition subword
                       ]
            count = foldr (\(_,_,l) r -> r + length l) 0
        in assert (count descs > count newDescs) $
           trace (show newDescs) $
           newDescs

constructNewRangeDescs2 :: [RangeDesc] -> ((RangeDesc,Int),(RangeDesc,Int)) -> Subword2 -> [RangeDesc]
constructNewRangeDescs2 d p s | trace ("constructNewRangeDescs " ++ show d ++ " " ++ show p ++ " " ++ show s) False = undefined
constructNewRangeDescs2 descs symbolPositions subword =
        let newDescs = [ newDesc |
                         desc <- descs
                       , newDesc <- processRangeDesc2 desc symbolPositions subword
                       ]
            count = foldr (\(_,_,l) r -> r + length l) 0
        in assert (count descs > count newDescs) $
           trace (show newDescs) $
           newDescs

processRangeDesc1 :: RangeDesc -> (RangeDesc,Int) -> Subword1 -> [RangeDesc]
processRangeDesc1 a b c | trace ("processRangeDesc1 " ++ show a ++ " " ++ show b ++ " " ++ show c) False = undefined
processRangeDesc1 inp (desc,aIdx) (m,n)
  | inp /= desc = [inp]
  | otherwise = processRangeDescSingle desc aIdx (m,n)

processRangeDesc2 :: RangeDesc -> ((RangeDesc,Int),(RangeDesc,Int)) -> Subword2 -> [RangeDesc]
processRangeDesc2 a b c | trace ("processRangeDesc2 " ++ show a ++ " " ++ show b ++ " " ++ show c) False = undefined
processRangeDesc2 inp ((left,a1Idx),(right,a2Idx)) (m,n,o,p)
  | inp /= left && inp /= right = [inp]
  | inp == left && inp == right =
        -- at this point it doesn't matter what the actual ordering is
        -- so we just swap if necessary to make it easier for processRangeDescDouble
        let (a1Idx',a2Idx',m',n',o',p') =
                if a1Idx < a2Idx then
                    (a1Idx,a2Idx,m,n,o,p)
                else
                    (a2Idx,a1Idx,o,p,m,n)
        in processRangeDescDouble inp a1Idx' a2Idx' (m',n',o',p')
  | inp == left = processRangeDescSingle left a1Idx (m,n)
  | inp == right = processRangeDescSingle right a2Idx (o,p)

filterEmptyRanges :: [RangeDesc] -> [RangeDesc]
filterEmptyRanges l =
        let f (i,j,d) = not $ null d && i == j
        in filter f l

processRangeDescSingle :: RangeDesc -> Int -> Subword1 -> [RangeDesc]
processRangeDescSingle a b c | trace ("processRangeDescSingle " ++ show a ++ " " ++ show b ++ " " ++ show c) False = undefined
processRangeDescSingle (i,j,r) aIdx (k,l)
  | aIdx == 0 = filterEmptyRanges [(l,j,tail r)]
  | aIdx == length r - 1 = [(i,k,init r)]
  | otherwise = [(i,k,take aIdx r),(l,j,drop (aIdx + 1) r)]

-- assumes that a1Idx < a2Idx, see processRangeDesc
processRangeDescDouble :: RangeDesc -> Int -> Int -> Subword2 -> [RangeDesc]
processRangeDescDouble a b c d | trace ("processRangeDescDouble " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d) False = undefined
processRangeDescDouble (i,j,r) a1Idx a2Idx (k,l,m,n) =
  assert (a1Idx < a2Idx) result where
  result | a1Idx == 0 && a2Idx == length r - 1 = filterEmptyRanges [(l,m,init (tail r))]
         | a1Idx == 0 = filterEmptyRanges [(l,m,slice 1 (a2Idx-1) r),(n,j,drop (a2Idx+1) r)]
         | a2Idx == length r - 1 = filterEmptyRanges [(i,k,take a1Idx r),(l,m,slice (a1Idx+1) (a2Idx-1) r)]
         | otherwise = filterEmptyRanges [(i,k,take a1Idx r),(l,m,slice (a1Idx+1) (a2Idx-1) r),(n,j,drop (a2Idx+1) r)]
    where slice from to xs = take (to - from + 1) (drop from xs)


infoFromPos :: InfoMap -> (RangeDesc,Int) -> Info
infoFromPos infoMap ((_,_,r),aIdx) =
        -- TODO !! might be expensive as it's a list
        infoMap Map.! (r !! aIdx)

-- calculates the combined yield size of all symbols left of the given one
combinedInfoLeftOf :: InfoMap -> (RangeDesc,Int) -> Info
combinedInfoLeftOf infoMap (desc,axIdx)
  | axIdx == 0 = (0, Just 0)
  | otherwise =
        let leftInfos = map (\i -> infoFromPos infoMap (desc,i)) [0..axIdx-1]
        in combineYields leftInfos

-- calculates the combined yield size of all symbols right of the given one
combinedInfoRightOf :: InfoMap -> (RangeDesc,Int) -> Info
combinedInfoRightOf infoMap (desc@(_,_,r),axIdx)
  | axIdx == length r - 1 = (0, Just 0)
  | otherwise =
        let rightInfos = map (\i -> infoFromPos infoMap (desc,i)) [axIdx+1..length r - 1]
        in combineYields rightInfos


calcSubwords2 :: InfoMap -> ((RangeDesc,Int),(RangeDesc,Int)) -> [Subword2]
calcSubwords2 a b | trace ("calcSubwords " ++ show a ++ " " ++ show b) False = undefined
calcSubwords2 infoMap (left@((i,j,r),a1Idx),right@((_,_,r'),a2Idx))
  | r == r' = calcSubwords2Dependent infoMap (i,j,r) a1Idx a2Idx
  | otherwise = [ (i',j',k',l') |
                  (i',j') <- calcSubwords1 infoMap left
                , (k',l') <- calcSubwords1 infoMap right
                ]

-- assumes that other component is in a different part
calcSubwords1 :: InfoMap -> (RangeDesc,Int) -> [Subword1]
calcSubwords1 _ b | trace ("calcSubwordsIndependent " ++ show b) False = undefined
calcSubwords1 infoMap pos@((i,j,_),_) =
        let (minY,maxY) = infoFromPos infoMap pos
            (minYLeft,maxYLeft) = combinedInfoLeftOf infoMap pos
            (minYRight,maxYRight) = combinedInfoRightOf infoMap pos
            model :: FDModel
            model = exists $ \col -> do
                  let rangeLen = fromIntegral (j-i)
                      [minY',minYLeft',minYRight'] = map fromIntegral [minY,minYLeft,minYRight]
                      [maxY',maxYLeft',maxYRight'] = map (maybe rangeLen fromIntegral) [maxY,maxYLeft,maxYRight]
                      -- TODO instead of using a safe default (rangeLen), it might be better not to
                      --      include a new inequality at all (how?)
                  [len1,len2,len3] <- colList col 3
                  xsum col @= rangeLen
                  len1 @>= minYLeft' 
                  len2 @>= minY'
                  len3 @>= minYRight'
                  len1 @<= maxYLeft'
                  len2 @<= maxY'
                  len3 @<= maxYRight'
                  rangeLen - maxYLeft'  @<= len2 + len3
                  rangeLen - maxYRight' @<= len1 + len2
                  rangeLen - maxY'      @<= len1 + len3
                  return col
        in map (\[len1,_,len3] -> (i+len1, j-len3)) $ solveModel model


calcSubwords2Dependent :: InfoMap -> RangeDesc -> Int -> Int -> [Subword2]
calcSubwords2Dependent _ b c d | trace ("calcSubwordsDependent " ++ show b ++ " " ++ show c ++ " " ++ show d) False = undefined
calcSubwords2Dependent infoMap desc a1Idx a2Idx =
        let a1Idx' = if a1Idx < a2Idx then a1Idx else a2Idx
            a2Idx' = if a1Idx < a2Idx then a2Idx else a1Idx
            subs = doCalcSubwords2Dependent infoMap desc a1Idx' a2Idx'
        in if a1Idx < a2Idx then subs
           else [ (k,l,m,n) | (m,n,k,l) <- subs ]

doCalcSubwords2Dependent :: InfoMap -> RangeDesc -> Int -> Int -> [Subword2]
doCalcSubwords2Dependent infoMap desc@(i,j,_) a1Idx a2Idx =
        let (minY1,maxY1) = infoFromPos infoMap (desc,a1Idx)
            (minY2,maxY2) = infoFromPos infoMap (desc,a2Idx)
            (minYLeft1,maxYLeft1) = combinedInfoLeftOf infoMap (desc,a1Idx)
            (minYRight1,maxYRight1) = combinedInfoRightOf infoMap (desc,a1Idx)
            (minYRight2,maxYRight2) = combinedInfoRightOf infoMap (desc,a2Idx)
            minYBetween = minYRight1 - minYRight2 - minY2
            maxYBetween | a1Idx + 1 == a2Idx = Just 0
                        | isNothing maxYRight1 = Nothing
                        | otherwise = Just $ fromJust maxYRight1 - fromJust maxYRight2 - fromJust maxY2
            model :: FDModel
            model = exists $ \col -> do
                  let rangeLen = fromIntegral (j-i)
                      [minYLeft1',minY1',minYBetween',minY2',minYRight2'] =
                          map fromIntegral [minYLeft1,minY1,minYBetween,minY2,minYRight2]
                      [maxYLeft1',maxY1',maxYBetween',maxY2',maxYRight2'] =
                          map (maybe rangeLen fromIntegral) [maxYLeft1,maxY1,maxYBetween,maxY2,maxYRight2]

                  [lenLeft1,len1,lenBetween,len2,lenRight2] <- colList col 5
                  xsum col @= rangeLen
                  lenLeft1   @>= minYLeft1'
                  len1       @>= minY1'
                  lenBetween @>= minYBetween'
                  len2       @>= minY2'
                  lenRight2  @>= minYRight2'
                  lenLeft1   @<= maxYLeft1'
                  len1       @<= maxY1'
                  lenBetween @<= maxYBetween'
                  len2       @<= maxY2'
                  lenRight2  @<= maxYRight2'
                  rangeLen - maxYLeft1'   @<= len1 + lenBetween + len2 + lenRight2
                  rangeLen - maxY1'       @<= lenLeft1 + lenBetween + len2 + lenRight2
                  rangeLen - maxYBetween' @<= lenLeft1 + len1 + len2 + lenRight2
                  rangeLen - maxY2'       @<= lenLeft1 + len1 + lenBetween + lenRight2
                  rangeLen - maxYRight2'  @<= lenLeft1 + len1 + lenBetween + len2
                  return col
        in map (\ [lenLeft1,len1,_,len2,lenRight2] ->
                  ( i + lenLeft1
                  , i + lenLeft1 + len1
                  , j - lenRight2 - len2
                  , j - lenRight2
                  )
               ) $ solveModel model
