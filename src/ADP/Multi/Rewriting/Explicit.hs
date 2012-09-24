{-# LANGUAGE FlexibleInstances #-}

module ADP.Multi.Rewriting.Explicit (
        determineYieldSize1,
        determineYieldSize2,
        constructRanges1,
        constructRanges2
) where

import Control.Exception
import Data.List (elemIndex, find)
import qualified Data.Map as Map
import Data.Maybe


import ADP.Debug
import ADP.Multi.Parser
import ADP.Multi.Rewriting
import ADP.Multi.Rewriting.YieldSize

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
constructNewRangeDescs1 d p s | trace ("constructNewRangeDescs1 " ++ show d ++ " " ++ show p ++ " " ++ show s) False = undefined
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
constructNewRangeDescs2 d p s | trace ("constructNewRangeDescs2 " ++ show d ++ " " ++ show p ++ " " ++ show s) False = undefined
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

-- Subword construction doesn't yet take the maximum yield sizes into account.
-- This will further decrease the number of generated subwords and thus increase performance.
calcSubwords2 :: InfoMap -> ((RangeDesc,Int),(RangeDesc,Int)) -> [Subword2]
calcSubwords2 a b | trace ("calcSubwords2 " ++ show a ++ " " ++ show b) False = undefined
calcSubwords2 infoMap (left@((i,j,r),a1Idx),right@((m,n,r'),a2Idx))
  | r == r' = calcSubwords2Dependent infoMap (i,j,r) a1Idx a2Idx
  | length r == 1 && length r' == 1 = [(i,j,m,n)]
  | length r == 1  = [ (i',j',k',l') |
                        let (i',j') = (i,j)
                     , (k',l') <- calcSubwords1 infoMap right
                     ]
  | length r' == 1 = [ (i',j',k',l') |
                       let (k',l') = (m,n)
                     , (i',j') <- calcSubwords1 infoMap left
                     ]
  | otherwise = [ (i',j',k',l') |
                  (i',j') <- calcSubwords1 infoMap left
                , (k',l') <- calcSubwords1 infoMap right
                ]

-- assumes that other component is in a different part
calcSubwords1 :: InfoMap -> (RangeDesc,Int) -> [Subword1]
calcSubwords1 _ b | trace ("calcSubwords1 " ++ show b) False = undefined
calcSubwords1 infoMap pos@((i,j,r),axIdx)
  | axIdx == 0 =
         [ (k,l) |
           Just (minY',minYRight') <- [adjustMinYield (i,j) (minY,minYRight) (maxY,maxYRight)]
         , let k = i
         , l <- [i+minY'..j-minYRight']
         ]
  | axIdx == length r - 1 =
         [ (k,l) |
           Just (minYLeft',minY') <- [adjustMinYield (i,j) (minYLeft,minY) (maxYLeft,maxY)]
         , let l = j
         , k <- [i+minYLeft'..j-minY']
         ]
  | otherwise =
        [ (k,l) |
          k <- [i+minYLeft..j-minY]
        , l <- [k+minY..j-minYRight]
        ]
  where (minY,maxY) = infoFromPos infoMap pos
        (minYLeft,maxYLeft) = combinedInfoLeftOf infoMap pos
        (minYRight,maxYRight) = combinedInfoRightOf infoMap pos

adjustMinYield :: Subword1 -> (Int,Int) -> (Maybe Int,Maybe Int) -> Maybe (Int,Int)
adjustMinYield (i,j) (minl,minr) (maxl,maxr) =
        let len = j-i
            adjust oldMinY maxY = let x = maybe oldMinY (\m -> len - m) maxY
                                  in if x > oldMinY then x else oldMinY
            minrAdj = adjust minr maxl
            minlAdj = adjust minl maxr
        in do
           minlRes <- maybe (Just minlAdj) (\m -> if minlAdj > m then Nothing else Just minlAdj) maxl
           minrRes <- maybe (Just minrAdj) (\m -> if minrAdj > m then Nothing else Just minrAdj) maxr
           Just (minlRes,minrRes)

-- assumes that other component is in the same part
calcSubwords2Dependent :: InfoMap -> RangeDesc -> Int -> Int -> [Subword2]
calcSubwords2Dependent _ b c d | trace ("calcSubwords2Dependent " ++ show b ++ " " ++ show c ++ " " ++ show d) False = undefined
calcSubwords2Dependent infoMap (i,j,r) a1Idx a2Idx =
        let a1Idx' = if a1Idx < a2Idx then a1Idx else a2Idx
            a2Idx' = if a1Idx < a2Idx then a2Idx else a1Idx
            subs = doCalcSubwords2Dependent infoMap (i,j,r) a1Idx' a2Idx'
        in if a1Idx < a2Idx then subs
           else [ (k,l,m,n) | (m,n,k,l) <- subs ]

doCalcSubwords2Dependent :: InfoMap -> RangeDesc -> Int -> Int -> [Subword2]
doCalcSubwords2Dependent infoMap desc@(i,j,r) a1Idx a2Idx =
   assert (a1Idx < a2Idx) $
   trace ("min yields: " ++ show minY1 ++ " " ++ show minY2 ++ " " ++ show minYLeft1 ++ " " ++
          show minYLeft2 ++ " " ++ show minYRight1 ++ " " ++ show minYRight2 ++ " " ++ show minYBetween) $
   trace ("max yields: " ++ show maxY1 ++ " " ++ show maxY2 ++ " " ++ show maxYLeft1 ++ " " ++
          show maxYLeft2 ++ " " ++ show maxYRight1 ++ " " ++ show maxYRight2 ++ " " ++ show maxYBetween) $
   result where

   (minY1,maxY1) = infoFromPos infoMap (desc,a1Idx)
   (minY2,maxY2) = infoFromPos infoMap (desc,a2Idx)
   (minYLeft1,maxYLeft1) = combinedInfoLeftOf infoMap (desc,a1Idx)
   (minYLeft2,maxYLeft2) = combinedInfoLeftOf infoMap (desc,a2Idx)
   (minYRight1,maxYRight1) = combinedInfoRightOf infoMap (desc,a1Idx)
   (minYRight2,maxYRight2) = combinedInfoRightOf infoMap (desc,a2Idx)
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
