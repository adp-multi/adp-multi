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
        constructRanges1,
        constructRanges2
) where

import Control.Exception
import qualified Data.Map as Map
import Data.Maybe (fromJust, isNothing)

import ADP.Debug
import ADP.Multi.Parser
import ADP.Multi.Rewriting
import ADP.Multi.Rewriting.YieldSize
import ADP.Multi.Rewriting.RangesHelper
import ADP.Multi.Rewriting.MonadicCpHelper
import Control.CP.FD.Interface


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
