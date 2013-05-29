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
            rangeDescs = [(i,j,left),(k,l,right)]
        in trace ("f " ++ show symbolIDs ++ " = (" ++ show left ++ "," ++ show right ++ ")") $
           assert (length left + length right == Map.size yieldSizeMap && 
                   all (`elem` (left ++ right)) symbolIDs &&
                   not (null left) && not (null right)) $
           constructSubwordsRec yieldSizeMap remainingParsers rangeDescs



constructSubwordsRec :: YieldSizeMap -> [(Int,ParserInfo)] -> [RangeDesc] -> [SubwordTree]
constructSubwordsRec a b c | trace ("constructSubwordsRec " ++ show a ++ " " ++ show b ++ " " ++ show c) False = undefined
constructSubwordsRec _ [] _ = []
constructSubwordsRec yieldSizeMap ((current,ParserInfo1 {}):rest) rangeDescs =
        let symbolPos = findSymbol1 current rangeDescs
            subwords = calcSubwords1 yieldSizeMap symbolPos
        in trace ("calc subwords for dim1") $
           trace ("subwords: " ++ show subwords) $
           [ SubwordTree [i,j] restTrees |
             (i,j) <- subwords,
             let newDescs = constructNewRangeDescs1 rangeDescs symbolPos (i,j),
             let restTrees = constructSubwordsRec yieldSizeMap rest newDescs
           ]
constructSubwordsRec yieldSizeMap ((current,ParserInfo2 {}):rest) rangeDescs =
        let symbolPositions = findSymbol2 current rangeDescs
            subwords = calcSubwords2 yieldSizeMap symbolPositions
        in trace ("calc subwords for dim2") $
           trace ("subwords: " ++ show subwords) $
           [ SubwordTree [i,j,k,l] restTrees |
             (i,j,k,l) <- subwords,
             let newDescs = constructNewRangeDescs2 rangeDescs symbolPositions (i,j,k,l),
             let restTrees = constructSubwordsRec yieldSizeMap rest newDescs
           ]



-- Subword construction doesn't yet take the maximum yield sizes into account.
-- This will further decrease the number of generated subwords and thus increase performance.
calcSubwords2 :: YieldSizeMap -> (SymbolPos,SymbolPos) -> [Subword2]
calcSubwords2 a b | trace ("calcSubwords2 " ++ show a ++ " " ++ show b) False = undefined
calcSubwords2 yieldSizeMap (left@((i,j,r),sym1Idx),right@((m,n,r'),sym2Idx))
  | r == r' = calcSubwords2Dependent yieldSizeMap (i,j,r) sym1Idx sym2Idx
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

calcSubwords1 :: YieldSizeMap -> SymbolPos -> [Subword1]
calcSubwords1 _ b | trace ("calcSubwords1 " ++ show b) False = undefined
calcSubwords1 yieldSizeMap pos@((i,j,r),symIdx)
  | symIdx == 0 =
         [ (k,l) |
           Just (minY',minYRight') <- [adjustMinYield (i,j) (minY,maxY) (minYRight,maxYRight)]
         , let k = i
         , l <- [i+minY'..j-minYRight']
         ]
  | symIdx == length r - 1 =
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
calcSubwords2Dependent yieldSizeMap (i,j,r) sym1Idx sym2Idx =
        let sym1Idx' = if sym1Idx < sym2Idx then sym1Idx else sym2Idx
            sym2Idx' = if sym1Idx < sym2Idx then sym2Idx else sym1Idx
            subs = doCalcSubwords2Dependent yieldSizeMap (i,j,r) sym1Idx' sym2Idx'
        in if sym1Idx < sym2Idx then subs
           else [ (k,l,m,n) | (m,n,k,l) <- subs ]

doCalcSubwords2Dependent :: YieldSizeMap -> RangeDesc -> Int -> Int -> [Subword2]
doCalcSubwords2Dependent yieldSizeMap desc@(i,j,r) sym1Idx sym2Idx =
   assert (sym1Idx < sym2Idx) $
   trace ("min yields: " ++ show minY1 ++ " " ++ show minY2 ++ " " ++ show minYLeft1 ++ " " ++
          show minYLeft2 ++ " " ++ show minYRight1 ++ " " ++ show minYRight2 ++ " " ++ show minYBetween) $
   trace ("max yields: " ++ show maxY1 ++ " " ++ show maxY2 ++ " " ++ show maxYLeft1 ++ " " ++
          show maxYLeft2 ++ " " ++ show maxYRight1 ++ " " ++ show maxYRight2 ++ " " ++ show maxYBetween) $
   result where

   (minY1,maxY1) = yieldSizeOf yieldSizeMap (desc,sym1Idx)
   (minY2,maxY2) = yieldSizeOf yieldSizeMap (desc,sym2Idx)
   (minYLeft1,maxYLeft1) = combinedYieldSizeLeftOf yieldSizeMap (desc,sym1Idx)
   (minYLeft2,maxYLeft2) = combinedYieldSizeLeftOf yieldSizeMap (desc,sym2Idx)
   (minYRight1,maxYRight1) = combinedYieldSizeRightOf yieldSizeMap (desc,sym1Idx)
   (minYRight2,maxYRight2) = combinedYieldSizeRightOf yieldSizeMap (desc,sym2Idx)
   minYBetween = minYRight1 - minYRight2 - minY2
   maxYBetween = if isNothing maxYRight1
                 then Nothing
                 else Just $ fromJust maxYRight1 - fromJust maxYRight2 - fromJust maxY2

   neighbors = sym1Idx + 1 == sym2Idx

   result | sym1Idx == 0 && sym2Idx == length r - 1 && neighbors =
                [ (k,l,l,n) |
                  let (k,n) = (i,j)
                , l <- [i+minY1..j-minY2]
                ]

          | sym1Idx == 0 && sym2Idx == length r - 1 =
                [ (k,l,m,n) |
                  let (k,n) = (i,j)
                , l <- [i+minY1..j-minYRight1]
                , m <- [l+minYBetween..j-minY2]
                ]

          | sym1Idx == 0 && neighbors =
                [ (k,l,l,n) |
                  let k = i
                , l <- [i+minY1..j-minYRight1]
                , n <- [l+minY2..j-minYRight2]
                ]

          | sym1Idx == 0 =
                [ (k,l,m,n) |
                  let k = i
                , l <- [i+minY1..j-minYRight1]
                , m <- [l+minYBetween..j-minY2-minYRight2]
                , n <- [m+minY2..j-minYRight2]
                ]

          | sym2Idx == length r - 1 && neighbors =
                [ (k,m,m,n) |
                  let n = j
                , m <- [i+minYLeft2..j-minY2]
                , k <- [i+minYLeft1..m-minY1]
                ]

          | sym2Idx == length r - 1 =
                [ (k,l,m,n) |
                  let n = j
                , m <- [i+minYLeft2..j-minY2]
                , l <- [i+minY1+minYLeft1..m-minYBetween]
                , k <- [i+minYLeft1..l-minY1]
                ]

          | sym1Idx > 0 && sym2Idx < length r - 1 && neighbors =
                [ (k,l,l,n) |
                  k <- [i+minYLeft1..j-minY1-minYRight1]
                , l <- [k+minY1..j-minYRight1]
                , n <- [l+minY2..j-minYRight2]
                ]

          | sym1Idx > 0 && sym2Idx < length r - 1 =
                [ (k,l,m,n) |
                  k <- [i+minYLeft1..j-minY1-minYRight1]
                , l <- [k+minY1..j-minYRight1]
                , m <- [l+minYBetween..j-minY2-minYRight2]
                , n <- [m+minY2..j-minYRight2]
                ]

          | otherwise = error "invalid conditions, e.g. sym1Idx == sym2Idx == 0"
