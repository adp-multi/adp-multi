{-# LANGUAGE FlexibleInstances #-}

module ADP.Multi.Rewriting.Explicit (
        constructSubwords1,
        constructSubwords2
) where

import Control.Exception
import qualified Data.Map as Map
import Data.Maybe

import ADP.Debug
import ADP.Multi.Parser
import ADP.Multi.Rewriting
import ADP.Multi.Rewriting.Model
import ADP.Multi.Rewriting.YieldSize
import ADP.Multi.Rewriting.RangesHelper

constructSubwords1 :: SubwordConstructionAlgorithm Dim1
constructSubwords1 _ _ b | trace ("constructSubwords1 " ++ show b) False = undefined
constructSubwords1 f infos [i,j] =
        assert (i <= j) $
        let yieldSizeMap = buildYieldSizeMap infos
            symbolIDs = Map.keys yieldSizeMap
            rewritten = f symbolIDs
            parserCount = length infos
            remainingParsers = [parserCount,parserCount-1..1] `zip` infos
            rangeDesc = [(i,j,rewritten)]
        in trace ("f " ++ show symbolIDs ++ " = " ++ show rewritten) $
           assert (length rewritten == Map.size yieldSizeMap && all (`elem` rewritten) symbolIDs) $
           constructSubwordsRec yieldSizeMap remainingParsers rangeDesc

constructSubwords2 :: SubwordConstructionAlgorithm Dim2
constructSubwords2 _ _ b | trace ("constructSubwords2 " ++ show b) False = undefined
constructSubwords2 f infos [i,j,k,l] =
        assert (i <= j && j <= k && k <= l) $
        let yieldSizeMap = buildYieldSizeMap infos
            symbolIDs = Map.keys yieldSizeMap
            (left,right) = f symbolIDs
            parserCount = length infos
            remainingParsers = [parserCount,parserCount-1..1] `zip` infos
            rangeDesc = [(i,j,left),(k,l,right)]
            rangeDescFiltered = filterEmptyRanges rangeDesc
        in trace ("f " ++ show symbolIDs ++ " = (" ++ show left ++ "," ++ show right ++ ")") $
           assert (length left + length right == Map.size yieldSizeMap && all (`elem` (left ++ right)) symbolIDs) $
           if any (\(m,n,d) -> null d && m /= n) rangeDesc then []
           else constructSubwordsRec yieldSizeMap remainingParsers rangeDescFiltered



constructSubwordsRec :: YieldSizeMap -> [(Int,ParserInfo)] -> [RangeDesc] -> [SubwordTree]
constructSubwordsRec a b c | trace ("constructRangesRec " ++ show a ++ " " ++ show b ++ " " ++ show c) False = undefined
constructSubwordsRec _ [] [] = []
constructSubwordsRec yieldSizeMap ((current,ParserInfo1 {}):rest) rangeDescs =
        let symbolLoc = findSymbol1 current rangeDescs
            subwords = calcSubwords1 yieldSizeMap symbolLoc
        in trace ("calc subwords for dim1") $
           trace ("subwords: " ++ show subwords) $
           [ SubwordTree [i,j] restTrees |
             (i,j) <- subwords,
             let newDescs = constructNewRangeDescs1 rangeDescs symbolLoc (i,j),
             let restTrees = constructSubwordsRec yieldSizeMap rest newDescs
           ]
constructSubwordsRec yieldSizeMap ((current,ParserInfo2 {}):rest) rangeDescs =
        let symbolLocs = findSymbol2 current rangeDescs
            subwords = calcSubwords2 yieldSizeMap symbolLocs
        in trace ("calc subwords for dim2") $
           trace ("subwords: " ++ show subwords) $
           [ SubwordTree [i,j,k,l] restTrees |
             (i,j,k,l) <- subwords,
             let newDescs = constructNewRangeDescs2 rangeDescs symbolLocs (i,j,k,l),
             let restTrees = constructSubwordsRec yieldSizeMap rest newDescs
           ]
constructSubwordsRec _ [] r@(_:_) = error ("programming error " ++ show r)



-- Subword construction doesn't yet take the maximum yield sizes into account.
-- This will further decrease the number of generated subwords and thus increase performance.
calcSubwords2 :: YieldSizeMap -> ((RangeDesc,Int),(RangeDesc,Int)) -> [Subword2]
calcSubwords2 a b | trace ("calcSubwords2 " ++ show a ++ " " ++ show b) False = undefined
calcSubwords2 yieldSizeMap (left@((i,j,r),a1Idx),right@((m,n,r'),a2Idx))
  | r == r' = calcSubwords2Dependent yieldSizeMap (i,j,r) a1Idx a2Idx
  | length r == 1 && length r' == 1 = [(i,j,m,n)]
  | length r == 1  = [ (i',j',k',l') |
                        let (i',j') = (i,j)
                     , (k',l') <- calcSubwords1 yieldSizeMap right
                     ]
  | length r' == 1 = [ (i',j',k',l') |
                       let (k',l') = (m,n)
                     , (i',j') <- calcSubwords1 yieldSizeMap left
                     ]
  | otherwise = [ (i',j',k',l') |
                  (i',j') <- calcSubwords1 yieldSizeMap left
                , (k',l') <- calcSubwords1 yieldSizeMap right
                ]

calcSubwords1 :: YieldSizeMap -> (RangeDesc,Int) -> [Subword1]
calcSubwords1 _ b | trace ("calcSubwords1 " ++ show b) False = undefined
calcSubwords1 yieldSizeMap pos@((i,j,r),axIdx)
  | axIdx == 0 =
         [ (k,l) |
           Just (minY',minYRight') <- [adjustMinYield (i,j) (minY,maxY) (minYRight,maxYRight)]
         , let k = i
         , l <- [i+minY'..j-minYRight']
         ]
  | axIdx == length r - 1 =
         [ (k,l) |
           Just (minYLeft',minY') <- [adjustMinYield (i,j) (minYLeft,maxYLeft) (minY,maxY)]
         , let l = j
         , k <- [i+minYLeft'..j-minY']
         ]
  | otherwise =
        [ (k,l) |
          k <- [i+minYLeft..j-minY]
        , l <- [k+minY..j-minYRight]
        ]
  where (minY,maxY) = yieldSizeOf yieldSizeMap pos
        (minYLeft,maxYLeft) = combinedYieldSizeLeftOf yieldSizeMap pos
        (minYRight,maxYRight) = combinedYieldSizeRightOf yieldSizeMap pos

adjustMinYield :: Subword1 -> YieldSize -> YieldSize -> Maybe (Int,Int)
adjustMinYield (i,j) (minl,maxl) (minr,maxr) =
        let len = j-i
            adjust oldMinY maxY = let x = maybe oldMinY (\m -> len - m) maxY
                                  in if x > oldMinY then x else oldMinY
            minrAdj = adjust minr maxl
            minlAdj = adjust minl maxr
        in do
           minlRes <- maybe (Just minlAdj) (\m -> if minlAdj > m then Nothing else Just minlAdj) maxl
           minrRes <- maybe (Just minrAdj) (\m -> if minrAdj > m then Nothing else Just minrAdj) maxr
           Just (minlRes,minrRes)

-- assumes that other nonterminal component is in the same part
calcSubwords2Dependent :: YieldSizeMap -> RangeDesc -> Int -> Int -> [Subword2]
calcSubwords2Dependent _ b c d | trace ("calcSubwords2Dependent " ++ show b ++ " " ++ show c ++ " " ++ show d) False = undefined
calcSubwords2Dependent yieldSizeMap (i,j,r) a1Idx a2Idx =
        let a1Idx' = if a1Idx < a2Idx then a1Idx else a2Idx
            a2Idx' = if a1Idx < a2Idx then a2Idx else a1Idx
            subs = doCalcSubwords2Dependent yieldSizeMap (i,j,r) a1Idx' a2Idx'
        in if a1Idx < a2Idx then subs
           else [ (k,l,m,n) | (m,n,k,l) <- subs ]

doCalcSubwords2Dependent :: YieldSizeMap -> RangeDesc -> Int -> Int -> [Subword2]
doCalcSubwords2Dependent yieldSizeMap desc@(i,j,r) a1Idx a2Idx =
   assert (a1Idx < a2Idx) $
   trace ("min yields: " ++ show minY1 ++ " " ++ show minY2 ++ " " ++ show minYLeft1 ++ " " ++
          show minYLeft2 ++ " " ++ show minYRight1 ++ " " ++ show minYRight2 ++ " " ++ show minYBetween) $
   trace ("max yields: " ++ show maxY1 ++ " " ++ show maxY2 ++ " " ++ show maxYLeft1 ++ " " ++
          show maxYLeft2 ++ " " ++ show maxYRight1 ++ " " ++ show maxYRight2 ++ " " ++ show maxYBetween) $
   result where

   (minY1,maxY1) = yieldSizeOf yieldSizeMap (desc,a1Idx)
   (minY2,maxY2) = yieldSizeOf yieldSizeMap (desc,a2Idx)
   (minYLeft1,maxYLeft1) = combinedYieldSizeLeftOf yieldSizeMap (desc,a1Idx)
   (minYLeft2,maxYLeft2) = combinedYieldSizeLeftOf yieldSizeMap (desc,a2Idx)
   (minYRight1,maxYRight1) = combinedYieldSizeRightOf yieldSizeMap (desc,a1Idx)
   (minYRight2,maxYRight2) = combinedYieldSizeRightOf yieldSizeMap (desc,a2Idx)
   minYBetween = minYRight1 - minYRight2 - minY2
   maxYBetween = if isNothing maxYRight1
                 then Nothing
                 else Just $ fromJust maxYRight1 - fromJust maxYRight2 - fromJust maxY2

   neighbors = a1Idx + 1 == a2Idx

   result | a1Idx == 0 && a2Idx == length r - 1 && neighbors =
                [ (k,l,l,n) |
                  let (k,n) = (i,j)
                , l <- [i+minY1..j-minY2]
                ]

          | a1Idx == 0 && a2Idx == length r - 1 =
                [ (k,l,m,n) |
                  let (k,n) = (i,j)
                , l <- [i+minY1..j-minYRight1]
                , m <- [l+minYBetween..j-minY2]
                ]

          | a1Idx == 0 && neighbors =
                [ (k,l,l,n) |
                  let k = i
                , l <- [i+minY1..j-minYRight1]
                , n <- [l+minY2..j-minYRight2]
                ]

          | a1Idx == 0 =
                [ (k,l,m,n) |
                  let k = i
                , l <- [i+minY1..j-minYRight1]
                , m <- [l+minYBetween..j-minY2-minYRight2]
                , n <- [m+minY2..j-minYRight2]
                ]

          | a2Idx == length r - 1 && neighbors =
                [ (k,m,m,n) |
                  let n = j
                , m <- [i+minYLeft2..j-minY2]
                , k <- [i+minYLeft1..m-minY1]
                ]

          | a2Idx == length r - 1 =
                [ (k,l,m,n) |
                  let n = j
                , m <- [i+minYLeft2..j-minY2]
                , l <- [i+minY1+minYLeft1..m-minYBetween]
                , k <- [i+minYLeft1..l-minY1]
                ]

          | a1Idx > 0 && a2Idx < length r - 1 && neighbors =
                [ (k,l,l,n) |
                  k <- [i+minYLeft1..j-minY1-minYRight1]
                , l <- [k+minY1..j-minYRight1]
                , n <- [l+minY2..j-minYRight2]
                ]

          | a1Idx > 0 && a2Idx < length r - 1 =
                [ (k,l,m,n) |
                  k <- [i+minYLeft1..j-minY1-minYRight1]
                , l <- [k+minY1..j-minYRight1]
                , m <- [l+minYBetween..j-minY2-minYRight2]
                , n <- [m+minY2..j-minYRight2]
                ]

          | otherwise = error "invalid conditions, e.g. a1Idx == a2Idx == 0"
