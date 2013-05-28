{-# OPTIONS_HADDOCK hide #-}

-- | Helper methods used for subword construction.
module ADP.Multi.Rewriting.RangesHelper where

import Control.Exception
import Data.List (elemIndex, find)
import qualified Data.Map as Map

import ADP.Debug
import ADP.Multi.Rewriting.Model
import ADP.Multi.Rewriting.YieldSize

-- an attempt to regain some type safety
type Subword1 = (Int,Int)
type Subword2 = (Int,Int,Int,Int)

-- | List of parser symbols and a start and end index over
--   which subwords shall be constructed.
--   Note: RangeDesc means Range Description. I don't like
--         that name very much, but haven't found a good alternative.
type RangeDesc = (Int,Int,[SymbolID])

-- | Searches for the given SymbolID in a list of RangeDesc's
--   and returns its index in the RangeDesc where it was found.  
findSymbol :: SymbolID -> [RangeDesc] -> (RangeDesc,Int)
findSymbol (s,idx) r | trace ("findSymbol " ++ show s ++ "," ++ show idx ++ " " ++ show r) False = undefined
findSymbol (s,idx) rangeDesc =
         let Just (i,j,r) = find (\(_,_,l') -> any (\(s',i') -> s' == s && i' == idx) l') rangeDesc
             Just aIdx = elemIndex (s,idx) r
         in ((i,j,r),aIdx)

findSymbol1 :: Int -> [RangeDesc] -> (RangeDesc,Int)
findSymbol1 s = findSymbol (s,1)

findSymbol2 :: Int -> [RangeDesc] -> ((RangeDesc,Int),(RangeDesc,Int))
findSymbol2 s rangeDesc = (findSymbol (s,1) rangeDesc, findSymbol (s,2) rangeDesc)

constructNewRangeDescs1 :: [RangeDesc] -> (RangeDesc,Int) -> Subword1 -> [RangeDesc]
constructNewRangeDescs1 d p s | trace ("constructNewRangeDescs1 " ++ show d ++ " " ++ show p ++ " " ++ show s) False = undefined
constructNewRangeDescs1 descs symbolPosition subword =
        let newDescs = [ newDesc |
                         desc <- descs
                       , newDesc <- processRangeDesc1 desc symbolPosition subword
                       ]
            countSymbols = foldr (\(_,_,l) r -> r + length l) 0
        in assert (countSymbols descs > countSymbols newDescs) $
           trace (show newDescs) $
           newDescs

constructNewRangeDescs2 :: [RangeDesc] -> ((RangeDesc,Int),(RangeDesc,Int)) -> Subword2 -> [RangeDesc]
constructNewRangeDescs2 d p s | trace ("constructNewRangeDescs2 " ++ show d ++ " " ++ show p ++ " " ++ show s) False = undefined
constructNewRangeDescs2 descs symbolPositions subword =
        let newDescs = [ newDesc |
                         desc <- descs
                       , newDesc <- processRangeDesc2 desc symbolPositions subword
                       ]
            countSymbols = foldr (\(_,_,l) r -> r + length l) 0
        in assert (countSymbols descs > countSymbols newDescs) $
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


-- | Returns the yield size of the symbol at the given index in
--   the given RangeDesc. 
yieldSizeOf :: YieldSizeMap -> (RangeDesc,Int) -> YieldSize
yieldSizeOf yieldSizeMap ((_,_,r),aIdx) =
        -- TODO !! might be expensive as it's a list
        yieldSizeMap Map.! (r !! aIdx)

-- | calculates the combined yield size of all symbols left of the given one
combinedYieldSizeLeftOf :: YieldSizeMap -> (RangeDesc,Int) -> YieldSize
combinedYieldSizeLeftOf yieldSizeMap (desc,axIdx)
  | axIdx == 0 = (0, Just 0)
  | otherwise =
        let leftYieldSizes = map (\i -> yieldSizeOf yieldSizeMap (desc,i)) [0..axIdx-1]
        in combineYields leftYieldSizes

-- | calculates the combined yield size of all symbols right of the given one
combinedYieldSizeRightOf :: YieldSizeMap -> (RangeDesc,Int) -> YieldSize
combinedYieldSizeRightOf yieldSizeMap (desc@(_,_,r),axIdx)
  | axIdx == length r - 1 = (0, Just 0)
  | otherwise =
        let rightYieldSizes = map (\i -> yieldSizeOf yieldSizeMap (desc,i)) [axIdx+1..length r - 1]
        in combineYields rightYieldSizes