{-# LANGUAGE FlexibleInstances #-}

module ADP.Multi.Rewriting.Simple where

import Debug.HTrace (htrace)
import Control.Exception
import Data.Maybe
import Data.List (find, elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map

import ADP.Multi.Parser
import ADP.Multi.Rewriting

trace _ b = b
--trace = htrace

-- 2-dim to 2-dim
instance Rewriting ([(Int,Int)] -> ([(Int,Int)],[(Int,Int)])) where
  constructRanges _ _ b | trace ("constructRanges2 " ++ show b) False = undefined
  constructRanges f infos (i,j,k,l) =
        assert (i <= j && j <= k && k <= l) $
        let parserCount = length infos
            args = concatMap (\ x -> [(x,1),(x,2)]) [1..parserCount] 
            (left,right) = f args
            remainingSymbols = [parserCount,parserCount-1..1]
            rangeDesc = [(i,j,left),(k,l,right)]
            rangeDescFiltered = filterEmptyRanges rangeDesc
        in if any (\(m,n,d) -> null d && m /= n) rangeDesc then []
           else constructRangesRec (buildInfoMap infos) remainingSymbols rangeDescFiltered
  determineYieldSize _ infos | trace ("determineYieldSize2 " ++ show infos) False = undefined
  determineYieldSize f infos =
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

type RangeDesc = (Int,Int,[(Int,Int)])

constructRangesRec :: InfoMap -> [Int] -> [RangeDesc] -> [Ranges]
constructRangesRec a b c | trace ("constructRangesRec " ++ show a ++ " " ++ show b ++ " " ++ show c) False = undefined
constructRangesRec _ [] [] = []
constructRangesRec infoMap (current:rest) rangeDescs =
        let symbolLoc = findSymbol current rangeDescs
            subwords = calcSubwords infoMap symbolLoc
        in trace ("subwords: " ++ show subwords) $
           [ RangeMap subword restRanges |
             subword <- subwords,
             let newDescs = constructNewRangeDescs rangeDescs symbolLoc subword,
             let restRanges = constructRangesRec infoMap rest newDescs
           ]
constructRangesRec _ [] r@(_:_) = error ("programming error " ++ show r)

findSymbol :: Int -> [RangeDesc] -> ((RangeDesc,Int),(RangeDesc,Int))
findSymbol s r | trace ("findSymbol " ++ show s ++ " " ++ show r) False = undefined
findSymbol s rangeDesc =
         let Just (i,j,r)  = find (\(_,_,l') -> any (\(s',i') -> s' == s && i' == 1) l') rangeDesc
             Just (m,n,r') = find (\(_,_,l') -> any (\(s',i') -> s' == s && i' == 2) l') rangeDesc
             Just a1Idx = elemIndex (s,1) r
             Just a2Idx = elemIndex (s,2) r'
         in (((i,j,r),a1Idx),((m,n,r'),a2Idx))

constructNewRangeDescs :: [RangeDesc] -> ((RangeDesc,Int),(RangeDesc,Int)) -> Subword2 -> [RangeDesc]
constructNewRangeDescs d p s | trace ("constructNewRangeDescs " ++ show d ++ " " ++ show p ++ " " ++ show s) False = undefined
constructNewRangeDescs descs symbolPositions subword =
        let newDescs = [ newDesc |
                         desc <- descs
                       , newDesc <- processRangeDesc desc symbolPositions subword
                       ]
            count = foldr (\(_,_,l) r -> r + length l) 0
        in assert (count descs > count newDescs) $
           trace (show newDescs) $
           newDescs

processRangeDesc :: RangeDesc -> ((RangeDesc,Int),(RangeDesc,Int)) -> Subword2 -> [RangeDesc]
processRangeDesc a b c | trace ("processRangeDesc " ++ show a ++ " " ++ show b ++ " " ++ show c) False = undefined
processRangeDesc inp ((left,a1Idx),(right,a2Idx)) (m,n,o,p)
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

processRangeDescSingle :: RangeDesc -> Int -> Subword -> [RangeDesc]
processRangeDescSingle a b c | trace ("processRangeDescSingle " ++ show a ++ " " ++ show b ++ " " ++ show c) False = undefined
processRangeDescSingle (i,j,r) aIdx (k,l)
  | aIdx == 0 = filterEmptyRanges [(l,j,tail r)]
  | aIdx == length r - 1 = [(i,k,init r)]
  | otherwise = [(i,k,take aIdx r),(l,j,drop (aIdx + 1) r)]

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

-- assumes that a1Idx < a2Idx, see processRangeDesc
processRangeDescDouble :: RangeDesc -> Int -> Int -> Subword2 -> [RangeDesc]
processRangeDescDouble a b c d | trace ("processRangeDescDouble " ++ show a ++ " " ++ show b ++ " " ++ show c ++ " " ++ show d) False = undefined
processRangeDescDouble (i,j,r) a1Idx a2Idx (k,l,m,n) = 
  assert (a1Idx < a2Idx) result where 
  result | a1Idx == 0 && a2Idx == length r - 1 = filterEmptyRanges [(l,m,init (tail r))]
         | a1Idx == 0 = filterEmptyRanges [(l,m,slice 1 (a2Idx-1) r),(n,j,drop (a2Idx+1) r)]
         | a2Idx == length r - 1 = filterEmptyRanges [(i,k,take a1Idx r),(l,m,slice (a1Idx+1) (a2Idx-1) r)]
         | otherwise = filterEmptyRanges [(i,k,take a1Idx r),(l,m,slice (a1Idx+1) (a2Idx-1) r),(n,j,drop (a2Idx+1) r)]

-- Subword construction doesn't yet take the maximum yield sizes into account.
-- This will further decrease the number of generated subwords and thus increase performance.
calcSubwords :: InfoMap -> ((RangeDesc,Int),(RangeDesc,Int)) -> [Subword2]
calcSubwords a b | trace ("calcSubwords " ++ show a ++ " " ++ show b) False = undefined
calcSubwords infoMap (left@((i,j,r),a1Idx),right@((m,n,r'),a2Idx))
  | i == m && j == n = calcSubwordsDependent infoMap (i,j,r) a1Idx a2Idx
  | length r == 1 && length r' == 1 = [(i,j,m,n)]
  | length r == 1  = [ (i',j',k',l') |
                        let (i',j') = (i,j)
                     , (k',l') <- calcSubwordsIndependent infoMap right
                     ]
  | length r' == 1 = [ (i',j',k',l') |
                       let (k',l') = (m,n)
                     , (i',j') <- calcSubwordsIndependent infoMap left
                     ]
  | otherwise = [ (i',j',k',l') |
                  (i',j') <- calcSubwordsIndependent infoMap left
                , (k',l') <- calcSubwordsIndependent infoMap right
                ]

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
        
-- assumes that other component is in a different part
calcSubwordsIndependent :: InfoMap -> (RangeDesc,Int) -> [Subword]
calcSubwordsIndependent _ b | trace ("calcSubwordsIndependent " ++ show b) False = undefined
calcSubwordsIndependent infoMap pos@((i,j,r),axIdx)
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
        
adjustMinYield :: Subword -> (Int,Int) -> (Maybe Int,Maybe Int) -> Maybe (Int,Int)
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
calcSubwordsDependent :: InfoMap -> RangeDesc -> Int -> Int -> [Subword2]
calcSubwordsDependent _ b c d | trace ("calcSubwordsDependent " ++ show b ++ " " ++ show c ++ " " ++ show d) False = undefined
calcSubwordsDependent infoMap (i,j,r) a1Idx a2Idx =
        let a1Idx' = if a1Idx < a2Idx then a1Idx else a2Idx
            a2Idx' = if a1Idx < a2Idx then a2Idx else a1Idx
            subs = doCalcSubwordsDependent infoMap (i,j,r) a1Idx' a2Idx'
        in if a1Idx < a2Idx then subs
           else [ (k,l,m,n) | (m,n,k,l) <- subs ]
 
doCalcSubwordsDependent :: InfoMap -> RangeDesc -> Int -> Int -> [Subword2]
doCalcSubwordsDependent infoMap desc@(i,j,r) a1Idx a2Idx =
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
       
         
-- 2-dim to 1-dim
-- not possible yet, see comment at ~~~
instance Rewriting ((a,a) -> [a]) where
  constructRanges f subword = undefined